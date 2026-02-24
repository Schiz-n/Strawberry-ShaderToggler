#pragma once
#include <reshade.hpp>
#include <string>
#include <vector>
#include <cstdint>

namespace ShaderToggler
{
    struct CaptureRequest
    {
        uint32_t                    shaderHash;
        reshade::api::resource      stagingResource;
        reshade::api::resource_desc originalDesc;
        uint32_t                    slot;
        bool                        ready;   // true once GPU copy is done
    };

    class TextureExporter
    {
    public:
        void requestCapture(reshade::api::command_list* cmdList,
            reshade::api::device* device,
            uint32_t                    shaderHash);

        void processPendingCaptures(reshade::api::device* device,
            const std::string& exportFolder);

        void setSourceDescriptors(const std::vector<reshade::api::resource_view>& views)
        {
            _pendingViews = views;
        }

        bool hasPendingCapture() const { return _captureRequested; }
        void flagCapture() { _captureRequested = true; }
        void clearCapture() { _captureRequested = false; }

    private:
        bool                                         _captureRequested = false;
        uint32_t                                     _pendingShaderHash = 0;
        std::vector<reshade::api::resource_view>     _pendingViews;
        std::vector<CaptureRequest>                  _pendingCaptures;

        void writePng(const std::string& path, uint32_t width, uint32_t height,
            const uint8_t* rgba, uint32_t rowPitch);
    };
}