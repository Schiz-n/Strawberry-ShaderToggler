#include "TextureExporter.h"
#define STBI_MSC_SECURE_CRT
#pragma warning(disable : 4996)
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
#pragma warning(default : 4996)
#include <fstream>

using namespace reshade::api;

namespace ShaderToggler
{
    void TextureExporter::requestCapture(command_list* cmdList,
        device* dev,
        uint32_t      shaderHash)
    {
        _pendingCaptures.clear();
        _pendingShaderHash = shaderHash;

        for (uint32_t slot = 0; slot < _pendingViews.size(); slot++)
        {
            const resource_view rv = _pendingViews[slot];
            if (rv.handle == 0) continue;

            // Get the underlying resource from the view
            resource res = dev->get_resource_from_view(rv);
            if (res.handle == 0) continue;

            resource_desc desc = dev->get_resource_desc(res);

            // Only handle 2D textures, skip depth, compressed, or exotic formats
            if (desc.type != resource_type::texture_2d) continue;
            if (desc.texture.width == 0 || desc.texture.height == 0) continue;

            // We can only readback uncompressed formats easily - skip BC formats
            const auto fmt = desc.texture.format;
            if (fmt == format::bc1_unorm || fmt == format::bc1_unorm_srgb ||
                fmt == format::bc2_unorm || fmt == format::bc2_unorm_srgb ||
                fmt == format::bc3_unorm || fmt == format::bc3_unorm_srgb ||
                fmt == format::bc4_unorm || fmt == format::bc4_snorm ||
                fmt == format::bc5_unorm || fmt == format::bc5_snorm ||
                fmt == format::bc6h_ufloat || fmt == format::bc7_unorm ||
                fmt == format::bc7_unorm_srgb) continue;

            // Create a staging resource (CPU readable)
            resource_desc stagingDesc = desc;
            stagingDesc.heap = memory_heap::gpu_to_cpu;
            stagingDesc.usage = resource_usage::copy_dest;
            stagingDesc.texture.levels = 1;  // mip 0 only

            resource staging = {};
            if (!dev->create_resource(stagingDesc, nullptr, resource_usage::copy_dest, &staging))
                continue;

            // Issue GPU copy — mip 0, array layer 0
            cmdList->copy_texture_region(
                res, 0, nullptr,
                staging, 0, nullptr);

            CaptureRequest req;
            req.shaderHash = shaderHash;
            req.stagingResource = staging;
            req.originalDesc = desc;
            req.slot = slot;
            req.ready = true;  // will be ready by next present
            _pendingCaptures.push_back(req);
        }

        _captureRequested = false;
    }


    void TextureExporter::processPendingCaptures(device* dev,
        const std::string& exportFolder)
    {
        for (auto& req : _pendingCaptures)
        {
            if (!req.ready) continue;

            const uint32_t w = req.originalDesc.texture.width;
            const uint32_t h = req.originalDesc.texture.height;

            // Map the staging resource
            reshade::api::subresource_data mapped = {};
            if (!dev->map_texture_region(req.stagingResource, 0, nullptr,
                map_access::read_only, &mapped))
            {
                dev->destroy_resource(req.stagingResource);
                continue;
            }

            // Row pitch can be padded — ask the API
            const uint32_t rowPitch = req.originalDesc.texture.width * 4;

            const std::string path = exportFolder
                + "ShaderToggler_0x" + [&]() {
                char buf[16]; sprintf_s(buf, sizeof(buf), "%08X", req.shaderHash);
                return std::string(buf); }()
                    + "_slot" + std::to_string(req.slot) + ".png";

                writePng(path, w, h, static_cast<const uint8_t*>(mapped.data), mapped.row_pitch);

                dev->unmap_texture_region(req.stagingResource, 0);
                dev->destroy_resource(req.stagingResource);
        }

        _pendingCaptures.clear();
    }


    void TextureExporter::writePng(const std::string& path,
        uint32_t           width,
        uint32_t           height,
        const uint8_t* data,
        uint32_t           rowPitch)
    {
        // stb_image_write handles stride natively via the stride parameter
        stbi_write_png(path.c_str(),
            static_cast<int>(width),
            static_cast<int>(height),
            4,       // assume RGBA — adjust if you hit single-channel textures
            data,
            static_cast<int>(rowPitch));
    }
}