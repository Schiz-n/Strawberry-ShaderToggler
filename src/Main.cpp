///////////////////////////////////////////////////////////////////////
//
// Part of ShaderToggler, a shader toggler add on for Reshade 5+ which allows you
// to define groups of shaders to toggle them on/off with one key press
// 
// (c) Frans 'Otis_Inf' Bouma.
//
// All rights reserved.
// https://github.com/FransBouma/ShaderToggler
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met :
//
//  * Redistributions of source code must retain the above copyright notice, this
//	  list of conditions and the following disclaimer.
//
//  * Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and / or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
/////////////////////////////////////////////////////////////////////////

#define IMGUI_DISABLE_INCLUDE_IMCONFIG_H
#define ImTextureID unsigned long long // Change ImGui texture ID type to that of a 'reshade::api::resource_view' handle

#include <imgui.h>
#include <reshade.hpp>
#include "crc32_hash.hpp"
#include "ShaderManager.h"
#include "CDataFile.h"
#include "ToggleGroup.h"
#include <vector>
#include <filesystem>
#include <chrono>
#include <unordered_set>
#include <cstdio>

using namespace reshade::api;
using namespace ShaderToggler;

extern "C" __declspec(dllexport) const char *NAME = "Shader Toggler";
extern "C" __declspec(dllexport) const char *DESCRIPTION = "Add-on which allows you to define groups of game shaders to toggle on/off with one key press.";

static constexpr uint32_t MAX_PS_SRV_SLOTS = 128;

struct __declspec(uuid("038B03AA-4C75-443B-A695-752D80797037")) CommandListDataContainer {
    uint64_t activePixelShaderPipeline;
    uint64_t activeVertexShaderPipeline;
	uint64_t activeComputeShaderPipeline;
    resource_view pixelSRVs[MAX_PS_SRV_SLOTS]; // current SRVs bound to the pixel-shader stage
};

#define FRAMECOUNT_COLLECTION_PHASE_DEFAULT 250;
#define HASH_FILE_NAME	"ShaderToggler.ini"

static ShaderToggler::ShaderManager g_pixelShaderManager;
static ShaderToggler::ShaderManager g_vertexShaderManager;
static ShaderToggler::ShaderManager g_computeShaderManager;
static KeyData g_keyCollector;
static atomic_uint32_t g_activeCollectorFrameCounter = 0;
static std::vector<ToggleGroup> g_toggleGroups;
static atomic_int g_toggleGroupIdKeyBindingEditing = -1;
static atomic_int g_toggleGroupIdShaderEditing = -1;
static float g_overlayOpacity = 1.0f;
static int g_startValueFramecountCollectionPhase = FRAMECOUNT_COLLECTION_PHASE_DEFAULT;
static std::string g_iniFileName = "";
// Key repeat state for shader hunting
struct KeyRepeatState {
	bool isHeld = false;
	std::chrono::steady_clock::time_point holdStart;
	std::chrono::steady_clock::time_point lastRepeat;
};

static int g_keyRepeatDelayMs = 500;  // milliseconds before repeat starts
static int g_keyRepeatIntervalMs = 125;  // milliseconds between repeats

static KeyRepeatState g_keyRepeat[9]; // one per numpad 1-9

// Texture-export state
static std::vector<resource_view> g_lastHuntedPixelSRVs;
static std::vector<std::string>   g_exportResultLines;
static bool                       g_showExportResult = false;

/// <summary>
/// Calculates a crc32 hash from the passed in shader bytecode. The hash is used to identity the shader in future runs.
/// </summary>
/// <param name="shaderData"></param>
/// <returns></returns>
static uint32_t calculateShaderHash(void* shaderData)
{
	if(nullptr==shaderData)
	{
		return 0;
	}

	const auto shaderDesc = *static_cast<shader_desc *>(shaderData);
	return compute_crc32(static_cast<const uint8_t *>(shaderDesc.code), shaderDesc.code_size);
}


/// <summary>
/// Adds a default group with VK_CAPITAL as toggle key. Only used if there aren't any groups defined in the ini file.
/// </summary>
void addDefaultGroup()
{
	ToggleGroup toAdd("Default", ToggleGroup::getNewGroupId());
	toAdd.setToggleKey(VK_CAPITAL, false, false, false);
	g_toggleGroups.push_back(toAdd);
}


/// <summary>
/// Loads the defined hashes and groups from the shaderToggler.ini file.
/// </summary>
void loadShaderTogglerIniFile()
{
	// Will assume it's started at the start of the application and therefore no groups are present.
	CDataFile iniFile;
	if(!iniFile.Load(g_iniFileName))
	{
		// not there
		return;
	}
	const float savedOpacity = iniFile.GetFloat("OverlayOpacity", "Settings");
	if (savedOpacity != FLT_MIN) g_overlayOpacity = savedOpacity;
	const int savedFrameCount = iniFile.GetInt("FramecountCollectionPhase", "Settings");
	if (savedFrameCount != INT_MIN) g_startValueFramecountCollectionPhase = savedFrameCount;
	const int savedRepeatDelay = iniFile.GetInt("KeyRepeatDelayMs", "Settings");
	if (savedRepeatDelay != INT_MIN) g_keyRepeatDelayMs = savedRepeatDelay;
	const int savedRepeatInterval = iniFile.GetInt("KeyRepeatIntervalMs", "Settings");
	if (savedRepeatInterval != INT_MIN) g_keyRepeatIntervalMs = savedRepeatInterval;
	int groupCounter = 0;
	const int numberOfGroups = iniFile.GetInt("AmountGroups", "General");
	if(numberOfGroups==INT_MIN)
	{
		// old format file?
		addDefaultGroup();
		groupCounter=-1;	// enforce old format read for pre 1.0 ini file.
	}
	else
	{
		for(int i=0;i<numberOfGroups;i++)
		{
			g_toggleGroups.push_back(ToggleGroup("", ToggleGroup::getNewGroupId()));
		}
	}
	for(auto& group: g_toggleGroups)
	{
		group.loadState(iniFile, groupCounter);		// groupCounter is normally 0 or greater. For when the old format is detected, it's -1 (and there's 1 group).
		groupCounter++;
	}
}


/// <summary>
/// Saves the currently known toggle groups with their shader hashes to the shadertoggler.ini file
/// </summary>
void saveShaderTogglerIniFile()
{
	// format: first section with # of groups, then per group a section with pixel and vertex shaders, as well as their name and key value.
	// groups are stored with "Group" + group counter, starting with 0.
	CDataFile iniFile;
	iniFile.SetFloat("OverlayOpacity", g_overlayOpacity, "", "Settings");
	iniFile.SetInt("FramecountCollectionPhase", g_startValueFramecountCollectionPhase, "", "Settings");
	iniFile.SetInt("KeyRepeatDelayMs", g_keyRepeatDelayMs, "", "Settings");
	iniFile.SetInt("KeyRepeatIntervalMs", g_keyRepeatIntervalMs, "", "Settings");
	iniFile.SetInt("AmountGroups", g_toggleGroups.size(), "",  "General");

	int groupCounter = 0;
	for(const auto& group: g_toggleGroups)
	{
		group.saveState(iniFile, groupCounter);
		groupCounter++;
	}
	iniFile.SetFileName(g_iniFileName);
	iniFile.Save();
}


static void onInitCommandList(command_list *commandList)
{
	commandList->create_private_data<CommandListDataContainer>();
}


static void onDestroyCommandList(command_list *commandList)
{
	commandList->destroy_private_data<CommandListDataContainer>();
}

static void onResetCommandList(command_list *commandList)
{
	CommandListDataContainer &commandListData = commandList->get_private_data<CommandListDataContainer>();
	commandListData.activePixelShaderPipeline = -1;
	commandListData.activeVertexShaderPipeline = -1;
	commandListData.activeComputeShaderPipeline = -1;
	memset(commandListData.pixelSRVs, 0, sizeof(commandListData.pixelSRVs));
}


static void onInitPipeline(device *device, pipeline_layout, uint32_t subobjectCount, const pipeline_subobject *subobjects, pipeline pipelineHandle)
{
	// shader has been created, we will now create a hash and store it with the handle we got.
	for (uint32_t i = 0; i < subobjectCount; ++i)
	{
		switch (subobjects[i].type)
		{
			case pipeline_subobject_type::vertex_shader:
				g_vertexShaderManager.addHashHandlePair(calculateShaderHash(subobjects[i].data), pipelineHandle.handle);
				break;
			case pipeline_subobject_type::pixel_shader:
				g_pixelShaderManager.addHashHandlePair(calculateShaderHash(subobjects[i].data), pipelineHandle.handle);
				break;
			case pipeline_subobject_type::compute_shader:
				g_computeShaderManager.addHashHandlePair(calculateShaderHash(subobjects[i].data), pipelineHandle.handle);
				break;
		}
	}
}


static void onDestroyPipeline(device *device, pipeline pipelineHandle)
{
	g_pixelShaderManager.removeHandle(pipelineHandle.handle);
	g_vertexShaderManager.removeHandle(pipelineHandle.handle);
	g_computeShaderManager.removeHandle(pipelineHandle.handle);
}


static void displayIsPartOfToggleGroup()
{
	ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 1.0f, 0.0f, 1.0f));
	ImGui::SameLine();
	ImGui::Text(" Shader is part of this toggle group.");
	ImGui::PopStyleColor();
}


static void displayShaderManagerInfo(ShaderManager& toDisplay, const char* shaderType)
{
	if(toDisplay.isInHuntingMode())
	{
		ImGui::Text("# of %s shaders active: %d. # of %s shaders in group: %d", shaderType, toDisplay.getAmountShaderHashesCollected(), shaderType, toDisplay.getMarkedShaderCount());
		// To this:
		ImGui::Text("Current selected %s shader: %d / %d  [Hash: 0x%08X]",
			shaderType,
			toDisplay.getActiveHuntedShaderIndex(),
			toDisplay.getAmountShaderHashesCollected(),
			toDisplay.getActiveHuntedShaderHash()   // you already have this getter
		);
		if(toDisplay.isHuntedShaderMarked())
		{
			displayIsPartOfToggleGroup();
		}
	}
}

static void displayShaderManagerStats(ShaderManager& toDisplay, const char* shaderType)
{
	ImGui::Text("# of pipelines with %s shaders: %d. # of different %s shaders gathered: %d.", shaderType, toDisplay.getPipelineCount(), shaderType, toDisplay.getShaderCount());
}


// -----------------------------------------------------------------------
// Texture-export helpers
// -----------------------------------------------------------------------

static std::string toHexStr(uint32_t v)
{
	char buf[16];
	snprintf(buf, sizeof(buf), "%08X", v);
	return std::string(buf);
}

// Write width×height RGBA8 pixels as an uncompressed PNG.
// Uses the zlib "stored" (no-compression) mode to avoid needing any library.
static bool writePng(const std::string& path, uint32_t w, uint32_t h, const uint8_t* rgba)
{
	// Build scanlines: filter byte (0 = None) followed by raw RGBA pixels
	const uint32_t rowBytes = w * 4;
	std::vector<uint8_t> raw(h * (rowBytes + 1));
	for (uint32_t y = 0; y < h; ++y)
	{
		raw[y * (rowBytes + 1)] = 0; // filter = None
		memcpy(raw.data() + y * (rowBytes + 1) + 1, rgba + (size_t)y * rowBytes, rowBytes);
	}

	// Adler-32 of all raw scanline bytes
	uint32_t a1 = 1, a2 = 0;
	for (const uint8_t b : raw) { a1 = (a1 + b) % 65521; a2 = (a2 + a1) % 65521; }
	const uint32_t adler = (a2 << 16) | a1;

	// Build zlib "stored" stream (CMF=0x78, FLG=0x01: 0x7801 % 31 == 0)
	std::vector<uint8_t> zlib;
	zlib.push_back(0x78);
	zlib.push_back(0x01);
	for (size_t remaining = raw.size(); remaining > 0; )
	{
		const uint16_t blk  = (uint16_t)std::min<size_t>(remaining, 65535);
		const uint16_t nblk = blk ^ 0xFFFFu;
		zlib.push_back(remaining <= 65535 ? 0x01 : 0x00); // BFINAL | BTYPE=00
		zlib.push_back(blk  & 0xFF); zlib.push_back((blk  >> 8) & 0xFF);
		zlib.push_back(nblk & 0xFF); zlib.push_back((nblk >> 8) & 0xFF);
		const uint8_t* ptr = raw.data() + (raw.size() - remaining);
		zlib.insert(zlib.end(), ptr, ptr + blk);
		remaining -= blk;
	}
	// Adler-32 big-endian
	zlib.push_back((adler >> 24) & 0xFF); zlib.push_back((adler >> 16) & 0xFF);
	zlib.push_back((adler >>  8) & 0xFF); zlib.push_back((adler      ) & 0xFF);

	// Assemble PNG
	std::vector<uint8_t> png;
	auto u32be = [](std::vector<uint8_t>& v, uint32_t n) {
		v.push_back((n>>24)&0xFF); v.push_back((n>>16)&0xFF);
		v.push_back((n>> 8)&0xFF); v.push_back((n    )&0xFF);
	};
	auto writeChunk = [&](const char* t, const uint8_t* d, uint32_t len) {
		u32be(png, len);
		const size_t start = png.size();
		png.push_back(t[0]); png.push_back(t[1]); png.push_back(t[2]); png.push_back(t[3]);
		if (len && d) png.insert(png.end(), d, d + len);
		u32be(png, compute_crc32(png.data() + start, 4 + len));
	};

	const uint8_t sig[8] = {137,80,78,71,13,10,26,10};
	png.insert(png.end(), sig, sig + 8);

	uint8_t ihdr[13];
	ihdr[0]=(w>>24)&0xFF; ihdr[1]=(w>>16)&0xFF; ihdr[2]=(w>>8)&0xFF; ihdr[3]=w&0xFF;
	ihdr[4]=(h>>24)&0xFF; ihdr[5]=(h>>16)&0xFF; ihdr[6]=(h>>8)&0xFF; ihdr[7]=h&0xFF;
	ihdr[8]=8; ihdr[9]=6; ihdr[10]=0; ihdr[11]=0; ihdr[12]=0;
	writeChunk("IHDR", ihdr, 13);
	writeChunk("IDAT", zlib.data(), (uint32_t)zlib.size());
	writeChunk("IEND", nullptr, 0);

	FILE* f = nullptr;
	if (fopen_s(&f, path.c_str(), "wb") != 0 || !f) return false;
	fwrite(png.data(), 1, png.size(), f);
	fclose(f);
	return true;
}

// Returns bytes per 4x4 block for BCn compressed formats, 0 if not block-compressed.
static uint32_t getBCBlockSize(format fmt)
{
	switch (fmt)
	{
	case format::bc1_typeless: case format::bc1_unorm: case format::bc1_unorm_srgb:
	case format::bc4_typeless: case format::bc4_unorm: case format::bc4_snorm:
		return 8;
	case format::bc2_typeless: case format::bc2_unorm: case format::bc2_unorm_srgb:
	case format::bc3_typeless: case format::bc3_unorm: case format::bc3_unorm_srgb:
	case format::bc5_typeless: case format::bc5_unorm: case format::bc5_snorm:
	case format::bc6h_typeless: case format::bc6h_ufloat: case format::bc6h_sfloat:
	case format::bc7_typeless: case format::bc7_unorm: case format::bc7_unorm_srgb:
		return 16;
	default:
		return 0;
	}
}

// Returns bytes per pixel for uncompressed textures, 0 if unknown.
static uint32_t getUncompressedBPP(format fmt)
{
	switch (fmt)
	{
	case format::r8_typeless: case format::r8_unorm: case format::r8_uint:
	case format::r8_snorm:    case format::r8_sint:
		return 1;
	case format::r8g8_typeless: case format::r8g8_unorm: case format::r8g8_uint:
	case format::r8g8_snorm:    case format::r8g8_sint:
	case format::r16_typeless:  case format::r16_float:  case format::r16_unorm:
	case format::r16_uint:      case format::r16_snorm:  case format::r16_sint:
		return 2;
	case format::r8g8b8a8_typeless:  case format::r8g8b8a8_unorm:      case format::r8g8b8a8_unorm_srgb:
	case format::r8g8b8a8_uint:      case format::r8g8b8a8_snorm:      case format::r8g8b8a8_sint:
	case format::b8g8r8a8_typeless:  case format::b8g8r8a8_unorm:      case format::b8g8r8a8_unorm_srgb:
	case format::b8g8r8x8_typeless:  case format::b8g8r8x8_unorm:      case format::b8g8r8x8_unorm_srgb:
	case format::r10g10b10a2_typeless: case format::r10g10b10a2_unorm: case format::r10g10b10a2_uint:
	case format::r10g10b10a2_xr_bias: case format::r11g11b10_float:    case format::r9g9b9e5:
	case format::r16g16_typeless:    case format::r16g16_float:        case format::r16g16_unorm:
	case format::r16g16_uint:        case format::r16g16_snorm:        case format::r16g16_sint:
	case format::r32_typeless:       case format::r32_float:           case format::r32_uint:
	case format::r32_sint:
		return 4;
	case format::r16g16b16a16_typeless: case format::r16g16b16a16_float: case format::r16g16b16a16_unorm:
	case format::r16g16b16a16_uint:     case format::r16g16b16a16_snorm: case format::r16g16b16a16_sint:
	case format::r32g32_typeless:       case format::r32g32_float:       case format::r32g32_uint:
	case format::r32g32_sint:
		return 8;
	case format::r32g32b32_typeless: case format::r32g32b32_float:
	case format::r32g32b32_uint:     case format::r32g32b32_sint:
		return 12;
	case format::r32g32b32a32_typeless: case format::r32g32b32a32_float:
	case format::r32g32b32a32_uint:     case format::r32g32b32a32_sint:
		return 16;
	default:
		return 0;
	}
}

// Write a 2D texture as a DDS file (DX10 extension, mip 0 only).
// data/rowPitch come directly from map_texture_region.
static bool writeDds(const std::string& path, uint32_t w, uint32_t h, format fmt,
                     const uint8_t* data, uint32_t rowPitch)
{
	const uint32_t dxgiFormat = (uint32_t)format_to_default_typed(fmt, 0);
	const uint32_t blockSz    = getBCBlockSize(fmt);
	const bool     isBC       = (blockSz > 0);
	uint32_t       numRows, tightPitch;
	if (isBC)
	{
		numRows    = (h + 3) / 4;
		tightPitch = (w + 3) / 4 * blockSz;
	}
	else
	{
		const uint32_t bpp = getUncompressedBPP(fmt);
		if (!bpp) return false;
		numRows    = h;
		tightPitch = w * bpp;
	}

	struct PF  { uint32_t size,flags,fourCC,rgbBits,rM,gM,bM,aM; };
	struct HDR { uint32_t size,flags,h,w,pitchLin,depth,mips; uint32_t res1[11]; PF pf; uint32_t caps,c2,c3,c4,res2; };
	struct DX10{ uint32_t fmt,dim,misc,arr,misc2; };

	HDR hdr  = {};
	hdr.size    = 124;
	hdr.flags   = 0x1u|0x2u|0x4u|0x1000u | (isBC ? 0x80000u : 0x8u);
	hdr.h       = h;  hdr.w = w;
	hdr.pitchLin = isBC ? numRows * tightPitch : tightPitch;
	hdr.mips    = 1;
	hdr.pf.size  = 32;
	hdr.pf.flags = 0x4u;
	hdr.pf.fourCC = 0x30315844u; // 'DX10'
	hdr.caps    = 0x1000u;

	DX10 dx10 = {};
	dx10.fmt  = dxgiFormat;
	dx10.dim  = 3; // D3D10_RESOURCE_DIMENSION_TEXTURE2D
	dx10.arr  = 1;

	FILE* f = nullptr;
	if (fopen_s(&f, path.c_str(), "wb") != 0 || !f) return false;
	const uint32_t magic = 0x20534444u;
	fwrite(&magic, 4, 1, f);
	fwrite(&hdr,  sizeof(hdr),  1, f);
	fwrite(&dx10, sizeof(dx10), 1, f);
	for (uint32_t row = 0; row < numRows; ++row)
		fwrite(data + (size_t)row * rowPitch, 1, tightPitch, f);
	fclose(f);
	return true;
}

// Export every unique 2D texture currently bound to the hunted pixel shader.
// RGBA8/BGRA8 → PNG; everything else (BCn, float, HDR, …) → DDS.
static std::vector<std::string> exportTextures(effect_runtime* runtime, uint32_t shaderHash)
{
	std::vector<std::string> results;

	if (g_lastHuntedPixelSRVs.empty())
	{
		results.push_back("Nothing to export - shader did not draw this frame.");
		return results;
	}

	device*        dev   = runtime->get_command_queue()->get_device();
	command_queue* queue = runtime->get_command_queue();
	command_list*  cmd   = queue->get_immediate_command_list();
	if (!cmd) { results.push_back("Error: no immediate command list."); return results; }

	const std::filesystem::path exportDir = std::filesystem::path(g_iniFileName).parent_path();
	std::unordered_set<uint64_t> seen;
	int n = 0;

	for (const resource_view& srv : g_lastHuntedPixelSRVs)
	{
		if (!srv.handle) continue;

		const resource res = dev->get_resource_from_view(srv);
		if (!res.handle || seen.count(res.handle)) continue;
		seen.insert(res.handle);

		const resource_desc desc = dev->get_resource_desc(res);
		if (desc.type != resource_type::texture_2d) continue;
		if (desc.texture.samples > 1) { results.push_back("Skipped MSAA texture."); continue; }

		const auto fmt     = desc.texture.format;
		const bool isBGRA8 = (fmt == format::b8g8r8a8_unorm     || fmt == format::b8g8r8a8_unorm_srgb
		                   || fmt == format::b8g8r8a8_typeless);
		const bool isRGBA8 = (fmt == format::r8g8b8a8_unorm     || fmt == format::r8g8b8a8_unorm_srgb
		                   || fmt == format::r8g8b8a8_typeless);
		if (!isBGRA8 && !isRGBA8)
		{
			results.push_back("Skipped: unsupported format " + std::to_string((int)fmt) + ".");
			continue;
		}

		const uint32_t w = desc.texture.width;
		const uint32_t h = desc.texture.height;

		// Create a CPU-readable staging texture
		resource staging = {0};
		const resource_desc stagingDesc(w, h, 1, 1, fmt, 1, memory_heap::gpu_to_cpu, resource_usage::copy_dest);
		if (!dev->create_resource(stagingDesc, nullptr, resource_usage::copy_dest, &staging))
		{
			results.push_back("Slot " + std::to_string(n) + ": staging alloc failed.");
			continue;
		}

		// Copy GPU texture → staging, flush, wait
		cmd->barrier(res, resource_usage::shader_resource, resource_usage::copy_source);
		cmd->copy_resource(res, staging);
		cmd->barrier(res, resource_usage::copy_source, resource_usage::shader_resource);
		queue->flush_immediate_command_list();
		queue->wait_idle();

		// Map staging and convert to RGBA8
		subresource_data data = {};
		if (dev->map_texture_region(staging, 0, nullptr, map_access::read_only, &data) && data.data)
		{
			std::vector<uint8_t> pixels((size_t)w * h * 4);
			const auto* src = static_cast<const uint8_t*>(data.data);
			for (uint32_t y = 0; y < h; ++y)
			{
				const uint8_t* row = src + (size_t)y * data.row_pitch;
				uint8_t*       dst = pixels.data() + (size_t)y * w * 4;
				if (isBGRA8)
					for (uint32_t x = 0; x < w; ++x)
					{ dst[x*4]=row[x*4+2]; dst[x*4+1]=row[x*4+1]; dst[x*4+2]=row[x*4+0]; dst[x*4+3]=row[x*4+3]; }
				else
					memcpy(dst, row, (size_t)w * 4);
			}
			dev->unmap_texture_region(staging, 0);

			const std::string fname = "ps_" + toHexStr(shaderHash) + "_" + std::to_string(n) + ".png";
			if (writePng((exportDir / fname).string(), w, h, pixels.data()))
				results.push_back(fname + "  (" + std::to_string(w) + "x" + std::to_string(h) + ")  exported.");
			else
				results.push_back(fname + ": write failed.");
			++n;
		}
		else
		{
			results.push_back("Slot " + std::to_string(n) + ": map failed.");
		}

		dev->destroy_resource(staging);
	}

	if (results.empty())
		results.push_back("No exportable textures found (all formats unsupported).");
	return results;
}

// Track resource views bound to the pixel-shader stage.
static void onPushDescriptors(command_list* cmdList, shader_stage stages, pipeline_layout, uint32_t,
                               const descriptor_set_update& update)
{
	if ((stages & shader_stage::pixel) == shader_stage::pixel
	    && update.type == descriptor_type::shader_resource_view
	    && update.count > 0 && update.descriptors)
	{
		auto& data = cmdList->get_private_data<CommandListDataContainer>();
		const resource_view* views = static_cast<const resource_view*>(update.descriptors);
		for (uint32_t i = 0; i < update.count; ++i)
		{
			const uint32_t slot = update.binding + i;
			if (slot < MAX_PS_SRV_SLOTS)
				data.pixelSRVs[slot] = views[i];
		}
	}
}

// If the draw call is using the currently hunted pixel shader, snapshot its SRVs.
static void captureHuntedShaderSRVs(command_list* commandList)
{
	if (!g_pixelShaderManager.isInHuntingMode() || g_activeCollectorFrameCounter > 0) return;
	const uint32_t huntedHash = g_pixelShaderManager.getActiveHuntedShaderHash();
	if (!huntedHash) return;

	const CommandListDataContainer& data    = commandList->get_private_data<CommandListDataContainer>();
	const uint32_t                  current = g_pixelShaderManager.getShaderHash(data.activePixelShaderPipeline);
	if (current != huntedHash) return;

	g_lastHuntedPixelSRVs.clear();
	for (uint32_t i = 0; i < MAX_PS_SRV_SLOTS; ++i)
		if (data.pixelSRVs[i].handle)
			g_lastHuntedPixelSRVs.push_back(data.pixelSRVs[i]);
}

static void onReshadeOverlay(reshade::api::effect_runtime *runtime)
{
	// Export-result popup
	if (g_showExportResult)
	{
		ImGui::SetNextWindowBgAlpha(0.92f);
		ImGui::SetNextWindowPos(ImVec2(10, 10), ImGuiCond_Always);
		if (ImGui::Begin("##ExportResult", nullptr,
		                 ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_AlwaysAutoResize |
		                 ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoMove))
		{
			ImGui::TextColored(ImVec4(0.4f, 1.0f, 0.4f, 1.0f), "Texture Export (Numpad 0)");
			ImGui::Separator();
			for (const auto& line : g_exportResultLines)
				ImGui::Text("%s", line.c_str());
			ImGui::Separator();
			if (ImGui::Button("  OK  "))
				g_showExportResult = false;
		}
		ImGui::End();
		return;
	}

	if(g_toggleGroupIdShaderEditing>=0)
	{
		ImGui::SetNextWindowBgAlpha(g_overlayOpacity);
		ImGui::SetNextWindowPos(ImVec2(10, 10));
		if (!ImGui::Begin("ShaderTogglerInfo", nullptr, ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoResize | 
														ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoSavedSettings))
		{
			ImGui::End();
			return;
		}
		string editingGroupName = "";
		for(auto& group:g_toggleGroups)
		{
			if(group.getId()==g_toggleGroupIdShaderEditing)
			{
				editingGroupName = group.getName();
				break;
			}
		}
		
		displayShaderManagerStats(g_vertexShaderManager, "vertex");
		displayShaderManagerStats(g_pixelShaderManager, "pixel");
		displayShaderManagerStats(g_computeShaderManager, "compute");

		if(g_activeCollectorFrameCounter > 0)
		{
			const uint32_t counterValue = g_activeCollectorFrameCounter;
			ImGui::Text("Collecting active shaders... frames to go: %d", counterValue);
		}
		else
		{
			if(g_vertexShaderManager.isInHuntingMode() || g_pixelShaderManager.isInHuntingMode() || g_computeShaderManager.isInHuntingMode())
			{
				ImGui::Text("Editing the shaders for group: %s", editingGroupName.c_str());
			}
			displayShaderManagerInfo(g_vertexShaderManager, "vertex");
			displayShaderManagerInfo(g_pixelShaderManager, "pixel");
			displayShaderManagerInfo(g_computeShaderManager, "compute");
		}
		ImGui::End();
	}
}


static void onBindPipeline(command_list* commandList, pipeline_stage stages, pipeline pipelineHandle)
{
	if(nullptr != commandList && pipelineHandle.handle != 0)
	{
		const bool handleHasPixelShaderAttached = g_pixelShaderManager.isKnownHandle(pipelineHandle.handle);
		const bool handleHasVertexShaderAttached = g_vertexShaderManager.isKnownHandle(pipelineHandle.handle);
		const bool handleHasComputeShaderAttached = g_computeShaderManager.isKnownHandle(pipelineHandle.handle);
		if(!handleHasPixelShaderAttached && !handleHasVertexShaderAttached && !handleHasComputeShaderAttached)
		{
			// draw call with unknown handle, don't collect it
			return;
		}
		CommandListDataContainer& commandListData = commandList->get_private_data<CommandListDataContainer>();
		// always do the following code as that has to run for every bind on a pipeline:
		if(g_activeCollectorFrameCounter > 0)
		{
			// in collection mode
			if(handleHasPixelShaderAttached)
			{
				g_pixelShaderManager.addActivePipelineHandle(pipelineHandle.handle);
			}
			if(handleHasVertexShaderAttached)
			{
				g_vertexShaderManager.addActivePipelineHandle(pipelineHandle.handle);
			}
			if(handleHasComputeShaderAttached)
			{
				g_computeShaderManager.addActivePipelineHandle(pipelineHandle.handle);
			}
		}
		else
		{
			commandListData.activePixelShaderPipeline = handleHasPixelShaderAttached ? pipelineHandle.handle : commandListData.activePixelShaderPipeline;
			commandListData.activeVertexShaderPipeline = handleHasVertexShaderAttached ? pipelineHandle.handle : commandListData.activeVertexShaderPipeline;
			commandListData.activeComputeShaderPipeline = handleHasComputeShaderAttached ? pipelineHandle.handle : commandListData.activeComputeShaderPipeline;
		}
		if((stages & pipeline_stage::pixel_shader) == pipeline_stage::pixel_shader)
		{
			if(handleHasPixelShaderAttached)
			{
				if(g_activeCollectorFrameCounter > 0)
				{
					// in collection mode
					g_pixelShaderManager.addActivePipelineHandle(pipelineHandle.handle);
				}
				commandListData.activePixelShaderPipeline = pipelineHandle.handle;
			}
		}
		if((stages & pipeline_stage::vertex_shader) == pipeline_stage::vertex_shader)
		{
			if(handleHasVertexShaderAttached)
			{
				if(g_activeCollectorFrameCounter > 0)
				{
					// in collection mode
					g_vertexShaderManager.addActivePipelineHandle(pipelineHandle.handle);
				}
				commandListData.activeVertexShaderPipeline = pipelineHandle.handle;
			}
		}
		if((stages & pipeline_stage::compute_shader) == pipeline_stage::compute_shader)
		{
			if(handleHasComputeShaderAttached)
			{
				if(g_activeCollectorFrameCounter > 0)
				{
					// in collection mode
					g_computeShaderManager.addActivePipelineHandle(pipelineHandle.handle);
				}
				commandListData.activeComputeShaderPipeline = pipelineHandle.handle;
			}
		}
	}
}


/// <summary>
/// This function will return true if the command list specified has one or more shader hashes which are currently marked to be hidden. Otherwise false.
/// </summary>
/// <param name="commandList"></param>
/// <returns>true if the draw call has to be blocked</returns>
bool blockDrawCallForCommandList(command_list* commandList)
{
	if(nullptr==commandList)
	{
		return false;
	}

	const CommandListDataContainer &commandListData = commandList->get_private_data<CommandListDataContainer>();
	uint32_t shaderHash = g_pixelShaderManager.getShaderHash(commandListData.activePixelShaderPipeline);
	bool blockCall = g_pixelShaderManager.isBlockedShader(shaderHash);
	for(auto& group : g_toggleGroups)
	{
		blockCall |= group.isBlockedPixelShader(shaderHash);
	}
	shaderHash = g_vertexShaderManager.getShaderHash(commandListData.activeVertexShaderPipeline);
	blockCall |= g_vertexShaderManager.isBlockedShader(shaderHash);
	for(auto& group : g_toggleGroups)
	{
		blockCall |= group.isBlockedVertexShader(shaderHash);
	}
	shaderHash = g_computeShaderManager.getShaderHash(commandListData.activeComputeShaderPipeline);
	blockCall |= g_computeShaderManager.isBlockedShader(shaderHash);
	for(auto& group : g_toggleGroups)
	{
		blockCall |= group.isBlockedComputeShader(shaderHash);
	}
	return blockCall;
}


static bool onDraw(command_list* commandList, uint32_t vertex_count, uint32_t instance_count, uint32_t first_vertex, uint32_t first_instance)
{
	captureHuntedShaderSRVs(commandList);
	return blockDrawCallForCommandList(commandList);
}


static bool onDrawIndexed(command_list* commandList, uint32_t index_count, uint32_t instance_count, uint32_t first_index, int32_t vertex_offset, uint32_t first_instance)
{
	captureHuntedShaderSRVs(commandList);
	return blockDrawCallForCommandList(commandList);
}


static bool onDrawOrDispatchIndirect(command_list* commandList, indirect_command type, resource buffer, uint64_t offset, uint32_t draw_count, uint32_t stride)
{
	switch(type)
	{
		case indirect_command::unknown:
		case indirect_command::draw:
		case indirect_command::draw_indexed:
		case indirect_command::dispatch:
			captureHuntedShaderSRVs(commandList);
			return blockDrawCallForCommandList(commandList);
		// the rest aren't blocked
	}
	return false;
}

// Helper — call once per frame per key
// keyIndex: 0-8 maps to VK_NUMPAD1 through VK_NUMPAD9
// action: the lambda to run when triggered
template<typename F>
static void handleHuntKey(effect_runtime* runtime, int vkKey, int keyIndex, F action)
{
	auto& state = g_keyRepeat[keyIndex];
	const bool isDown = runtime->is_key_down(vkKey);
	const auto now = std::chrono::steady_clock::now();

	if (!isDown) {
		state.isHeld = false;
		return;
	}
	if (!state.isHeld) {
		// Fresh press — fire immediately
		state.isHeld = true;
		state.holdStart = now;
		state.lastRepeat = now;
		action();
		return;
	}
	// Already held — check if past initial delay and repeat interval
	if ((now - state.holdStart) >= std::chrono::milliseconds(g_keyRepeatDelayMs) &&
		(now - state.lastRepeat) >= std::chrono::milliseconds(g_keyRepeatIntervalMs))
	{
		state.lastRepeat = now;
		action();
	}
}

static void onReshadePresent(effect_runtime* runtime)
{
	if(g_activeCollectorFrameCounter>0)
	{
		--g_activeCollectorFrameCounter;
	}

	for(auto& group: g_toggleGroups)
	{
		if(group.isToggleKeyPressed(runtime))
		{
			group.toggleActive();
			// if the group's shaders are being edited, it should toggle the ones currently marked.
			if(group.getId() == g_toggleGroupIdShaderEditing)
			{
				g_vertexShaderManager.toggleHideMarkedShaders();
				g_pixelShaderManager.toggleHideMarkedShaders();
				g_computeShaderManager.toggleHideMarkedShaders();
			}
		}
	}


	// hardcoded hunting keys.
	// If Ctrl is pressed too, it'll step to the next marked shader (if any)
	// Numpad 1: previous pixel shader
	// Numpad 2: next pixel shader
	// Numpad 3: mark current pixel shader as part of the toggle group
	// Numpad 4: previous vertex shader
	// Numpad 5: next vertex shader
	// Numpad 6: mark current vertex shader as part of the toggle group
	// Numpad 7: previous compute shader
	// Numpad 8: next compute shader
	// Numpad 9: mark current compute shader as part of the toggle group
	const bool ctrlDown = runtime->is_key_down(VK_CONTROL);

	handleHuntKey(runtime, VK_NUMPAD1, 0, [&]{ g_pixelShaderManager.huntPreviousShader(ctrlDown); });
	handleHuntKey(runtime, VK_NUMPAD2, 1, [&]{ g_pixelShaderManager.huntNextShader(ctrlDown); });
	handleHuntKey(runtime, VK_NUMPAD3, 2, [&]{ g_pixelShaderManager.toggleMarkOnHuntedShader(); });
	handleHuntKey(runtime, VK_NUMPAD4, 3, [&]{ g_vertexShaderManager.huntPreviousShader(ctrlDown); });
	handleHuntKey(runtime, VK_NUMPAD5, 4, [&]{ g_vertexShaderManager.huntNextShader(ctrlDown); });
	handleHuntKey(runtime, VK_NUMPAD6, 5, [&]{ g_vertexShaderManager.toggleMarkOnHuntedShader(); });
	handleHuntKey(runtime, VK_NUMPAD7, 6, [&]{ g_computeShaderManager.huntPreviousShader(ctrlDown); });
	handleHuntKey(runtime, VK_NUMPAD8, 7, [&]{ g_computeShaderManager.huntNextShader(ctrlDown); });
	handleHuntKey(runtime, VK_NUMPAD9, 8, [&]{ g_computeShaderManager.toggleMarkOnHuntedShader(); });

	// Numpad 0: export textures bound to the currently hunted pixel shader
	if (runtime->is_key_pressed(VK_NUMPAD0)
	    && g_pixelShaderManager.isInHuntingMode()
	    && g_activeCollectorFrameCounter == 0
	    && g_pixelShaderManager.getActiveHuntedShaderHash() != 0)
	{
		g_exportResultLines = exportTextures(runtime, g_pixelShaderManager.getActiveHuntedShaderHash());
		g_showExportResult  = true;
	}
}


/// <summary>
/// Function which marks the end of a keybinding editing cycle
/// </summary>
/// <param name="acceptCollectedBinding"></param>
/// <param name="groupEditing"></param>
void endKeyBindingEditing(bool acceptCollectedBinding, ToggleGroup& groupEditing)
{
	if (acceptCollectedBinding && g_toggleGroupIdKeyBindingEditing == groupEditing.getId() && g_keyCollector.isValid())
	{
		groupEditing.setToggleKey(g_keyCollector);
	}
	g_toggleGroupIdKeyBindingEditing = -1;
	g_keyCollector.clear();
}


/// <summary>
/// Function which marks the start of a keybinding editing cycle for the passed in toggle group
/// </summary>
/// <param name="groupEditing"></param>
void startKeyBindingEditing(ToggleGroup& groupEditing)
{
	if (g_toggleGroupIdKeyBindingEditing == groupEditing.getId())
	{
		return;
	}
	if (g_toggleGroupIdKeyBindingEditing >= 0)
	{
		endKeyBindingEditing(false, groupEditing);
	}
	g_toggleGroupIdKeyBindingEditing = groupEditing.getId();
}


/// <summary>
/// Function which marks the end of a shader editing cycle for a given toggle group
/// </summary>
/// <param name="acceptCollectedShaderHashes"></param>
/// <param name="groupEditing"></param>
void endShaderEditing(bool acceptCollectedShaderHashes, ToggleGroup& groupEditing)
{
	if(acceptCollectedShaderHashes && g_toggleGroupIdShaderEditing == groupEditing.getId())
	{
		groupEditing.storeCollectedHashes(g_pixelShaderManager.getMarkedShaderHashes(), g_vertexShaderManager.getMarkedShaderHashes(), g_computeShaderManager.getMarkedShaderHashes());
		g_pixelShaderManager.stopHuntingMode();
		g_vertexShaderManager.stopHuntingMode();
		g_computeShaderManager.stopHuntingMode();
	}
	g_toggleGroupIdShaderEditing = -1;
}


/// <summary>
/// Function which marks the start of a shader editing cycle for a given toggle group.
/// </summary>
/// <param name="groupEditing"></param>
void startShaderEditing(ToggleGroup& groupEditing)
{
	if(g_toggleGroupIdShaderEditing==groupEditing.getId())
	{
		return;
	}
	if(g_toggleGroupIdShaderEditing >= 0)
	{
		endShaderEditing(false, groupEditing);
	}
	g_toggleGroupIdShaderEditing = groupEditing.getId();
	g_activeCollectorFrameCounter = g_startValueFramecountCollectionPhase;
	g_pixelShaderManager.startHuntingMode(groupEditing.getPixelShaderHashes());
	g_vertexShaderManager.startHuntingMode(groupEditing.getVertexShaderHashes());
	g_computeShaderManager.startHuntingMode(groupEditing.getComputeShaderHashes());

	// after copying them to the managers, we can now clear the group's shader.
	groupEditing.clearHashes();
}


static void showHelpMarker(const char* desc)
{
	ImGui::TextDisabled("(?)");
	if (ImGui::IsItemHovered())
	{
		ImGui::BeginTooltip();
		ImGui::PushTextWrapPos(450.0f);
		ImGui::TextUnformatted(desc);
		ImGui::PopTextWrapPos();
		ImGui::EndTooltip();
	}
}


static void displaySettings(reshade::api::effect_runtime* runtime)
{
	if(g_toggleGroupIdKeyBindingEditing >= 0)
	{
		// a keybinding is being edited. Read current pressed keys into the collector, cumulatively;
		g_keyCollector.collectKeysPressed(runtime);
	}

	if(ImGui::CollapsingHeader("General info and help"))
	{
		ImGui::PushTextWrapPos();
		ImGui::TextUnformatted("The Shader Toggler allows you to create one or more groups with shaders to toggle on/off. You can assign a keyboard shortcut (including using keys like Shift, Alt and Control) to each group, including a handy name. Each group can have one or more vertex or pixel shaders assigned to it. When you press the assigned keyboard shortcut, any draw calls using these shaders will be disabled, effectively hiding the elements in the 3D scene.");
		ImGui::TextUnformatted("\nThe following (hardcoded) keyboard shortcuts are used when you click a group's 'Change Shaders' button:");
		ImGui::TextUnformatted("* Numpad 1 and Numpad 2: previous/next pixel shader");
		ImGui::TextUnformatted("* Ctrl + Numpad 1 and Ctrl + Numpad 2: previous/next marked pixel shader in the group");
		ImGui::TextUnformatted("* Numpad 3: mark/unmark the current pixel shader as being part of the group");
		ImGui::TextUnformatted("* Numpad 4 and Numpad 5: previous/next vertex shader");
		ImGui::TextUnformatted("* Ctrl + Numpad 4 and Ctrl + Numpad 5: previous/next marked vertex shader in the group");
		ImGui::TextUnformatted("* Numpad 6: mark/unmark the current vertex shader as being part of the group");
		ImGui::TextUnformatted("* Numpad 7 and Numpad 8: previous/next compute shader");
		ImGui::TextUnformatted("* Ctrl + Numpad 7 and Ctrl + Numpad 8: previous/next marked compute shader in the group");
		ImGui::TextUnformatted("* Numpad 9: mark/unmark the current compute shader as being part of the group");
		ImGui::TextUnformatted("* Numpad 0: export textures bound to the current pixel shader as PNG files");
		ImGui::TextUnformatted("\nWhen you step through the shaders, the current shader is disabled in the 3D scene so you can see if that's the shader you were looking for.");
		ImGui::TextUnformatted("When you're done, make sure you click 'Save all toggle groups' to preserve the groups you defined so next time you start your game they're loaded in and you can use them right away.");
		ImGui::PopTextWrapPos();
	}

	ImGui::AlignTextToFramePadding();
	if(ImGui::CollapsingHeader("Shader selection parameters", ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::AlignTextToFramePadding();
		ImGui::PushItemWidth(ImGui::GetWindowWidth() * 0.5f);
		ImGui::SliderFloat("Overlay opacity", &g_overlayOpacity, 0.2f, 1.0f);
		ImGui::AlignTextToFramePadding();
		ImGui::SliderInt("# of frames to collect", &g_startValueFramecountCollectionPhase, 10, 1000);
		ImGui::SameLine();
		showHelpMarker("This is the number of frames the addon will collect active shaders. Set this to a high number if the shader you want to mark is only used occasionally. Only shaders that are used in the frames collected can be marked.");
		ImGui::AlignTextToFramePadding();
		ImGui::SliderInt("Search hold delay (ms)", &g_keyRepeatDelayMs, 100, 1000);
		ImGui::SameLine();
		showHelpMarker("How long you must hold a numpad key before it starts repeating. Lower = faster response.");

		ImGui::AlignTextToFramePadding();
		ImGui::SliderInt("Search repeat speed (ms)", &g_keyRepeatIntervalMs, 20, 500);
		ImGui::SameLine();
		showHelpMarker("How quickly the shader steps repeat while holding. Lower = faster scrolling.");
		ImGui::PopItemWidth();
	}
	ImGui::Separator();

	if(ImGui::CollapsingHeader("List of Toggle Groups", ImGuiTreeNodeFlags_DefaultOpen))
	{
		if(ImGui::Button(" New "))
		{
			addDefaultGroup();
		}
		ImGui::Separator();

		std::vector<ToggleGroup> toRemove;
		std::vector<ToggleGroup> toAdd;
		for(auto& group : g_toggleGroups)
		{
			ImGui::PushID(group.getId());
			ImGui::AlignTextToFramePadding();
			if(ImGui::Button("X"))
			{
				toRemove.push_back(group);
			}
			ImGui::SameLine();
			ImGui::Text(" %d ", group.getId());
			ImGui::SameLine();
			if(ImGui::Button("Edit"))
			{
				group.setEditing(true);
			}
			ImGui::SameLine();
			if (g_toggleGroupIdShaderEditing >= 0)
			{
				ImGui::BeginDisabled(true);
				ImGui::Button(" Copy ");
				ImGui::EndDisabled();
			}
			else
			{
				if (ImGui::Button(" Copy "))
				{
					toAdd.push_back(group.createCopy());
				}
			}
			ImGui::SameLine();
			if(g_toggleGroupIdShaderEditing >= 0)
			{
				if(g_toggleGroupIdShaderEditing == group.getId())
				{
					if(ImGui::Button(" Done "))
					{
						endShaderEditing(true, group);
					}
				}
				else
				{
					ImGui::BeginDisabled(true);
					ImGui::Button("      ");
					ImGui::EndDisabled();
				}
			}
			else
			{
				if(ImGui::Button("Change shaders"))
				{
					ImGui::SameLine();
					startShaderEditing(group);
				}
			}
			ImGui::SameLine();
			ImGui::Text(" %s (%s%s)", group.getName().c_str(), group.getToggleKeyAsString().c_str(), group.isActive() ? ", is active" : "");
			if(group.isActiveAtStartup())
			{
				ImGui::SameLine();
				ImGui::Text(" (Active at startup)");
			}
			if(group.isEditing())
			{
				ImGui::Separator();
				ImGui::Text("Edit group %d", group.getId());

				// Name of group
				char tmpBuffer[150];
				const string& name = group.getName();
				strncpy_s(tmpBuffer, 150, name.c_str(), name.size());
				ImGui::PushItemWidth(ImGui::GetWindowWidth() * 0.7f);
				ImGui::AlignTextToFramePadding();
				ImGui::Text("Name");
				ImGui::SameLine(ImGui::GetWindowWidth() * 0.25f);
				ImGui::InputText("##Name", tmpBuffer, 149);
				group.setName(tmpBuffer);
				ImGui::PopItemWidth();

				// Key binding of group
				bool isKeyEditing = false;
				ImGui::PushItemWidth(ImGui::GetWindowWidth() * 0.5f);
				ImGui::AlignTextToFramePadding();
				ImGui::Text("Key shortcut");
				ImGui::SameLine(ImGui::GetWindowWidth() * 0.25f);
				string textBoxContents = (g_toggleGroupIdKeyBindingEditing == group.getId()) ? g_keyCollector.getKeyAsString() : group.getToggleKeyAsString();	// The 'press a key' is inside keycollector
				string toggleKeyName = group.getToggleKeyAsString();
				ImGui::InputText("##Key shortcut", (char*)textBoxContents.c_str(), textBoxContents.size(), ImGuiInputTextFlags_ReadOnly);
				if(ImGui::IsItemClicked())
				{
					startKeyBindingEditing(group);
				}
				if(g_toggleGroupIdKeyBindingEditing == group.getId())
				{
					isKeyEditing = true;
					ImGui::SameLine();
					if(ImGui::Button("OK"))
					{
						endKeyBindingEditing(true, group);
					}
					ImGui::SameLine();
					if(ImGui::Button("Cancel"))
					{
						endKeyBindingEditing(false, group);
					}
				}
				ImGui::PopItemWidth();

				ImGui::PushItemWidth(ImGui::GetWindowWidth() * 0.7f);
				ImGui::Text(" ");
				ImGui::SameLine(ImGui::GetWindowWidth() * 0.25f);
				bool isDefaultActive = group.isActiveAtStartup();
				ImGui::Checkbox("Is active at startup", &isDefaultActive);
				group.setIsActiveAtStartup(isDefaultActive);
				ImGui::PopItemWidth();

				if(!isKeyEditing)
				{
					if(ImGui::Button("OK"))
					{
						group.setEditing(false);
						g_toggleGroupIdKeyBindingEditing = -1;
						g_keyCollector.clear();
					}
				}
				ImGui::Separator();
			}

			ImGui::PopID();
		}
		if(toRemove.size() > 0)
		{
			// switch off keybinding editing or shader editing, if in progress
			g_toggleGroupIdKeyBindingEditing = -1;
			g_keyCollector.clear();
			g_toggleGroupIdShaderEditing = -1;
			g_pixelShaderManager.stopHuntingMode();
			g_vertexShaderManager.stopHuntingMode();
		}
		for(const auto& group : toRemove)
		{
			std::erase(g_toggleGroups, group);
		}
		for (auto& copy : toAdd)
		{
			g_toggleGroups.push_back(std::move(copy));
		}
		ImGui::Separator();
		if(g_toggleGroups.size() > 0)
		{
			if(ImGui::Button("Save all Toggle Groups"))
			{
				saveShaderTogglerIniFile();
			}
		}
	}
}


BOOL APIENTRY DllMain(HMODULE hModule, DWORD fdwReason, LPVOID)
{
	switch (fdwReason)
	{
	case DLL_PROCESS_ATTACH:
		{
			if(!reshade::register_addon(hModule))
			{
				return FALSE;
			}

			// We'll pass a nullptr for the module handle so we get the containing process' executable + path. We can't use the reshade's api as we don't have the runtime
			// and we can't use reshade's handle because under vulkan reshade is stored in a central space and therefore it won't get the folder of the exe (where the reshade dll is located as well).
			WCHAR buf[MAX_PATH];
			const std::filesystem::path dllPath = GetModuleFileNameW(nullptr, buf, ARRAYSIZE(buf)) ? buf : std::filesystem::path();		// <installpath>/shadertoggler.addon64
			const std::filesystem::path basePath = dllPath.parent_path();																// <installpath>
			const std::string& hashFileName = HASH_FILE_NAME;
			g_iniFileName = (basePath / hashFileName).string();																			// <installpath>/shadertoggler.ini
			reshade::register_event<reshade::addon_event::init_pipeline>(onInitPipeline);
			reshade::register_event<reshade::addon_event::init_command_list>(onInitCommandList);
			reshade::register_event<reshade::addon_event::destroy_command_list>(onDestroyCommandList);
			reshade::register_event<reshade::addon_event::reset_command_list>(onResetCommandList);
			reshade::register_event<reshade::addon_event::destroy_pipeline>(onDestroyPipeline);
			reshade::register_event<reshade::addon_event::reshade_overlay>(onReshadeOverlay);
			reshade::register_event<reshade::addon_event::reshade_present>(onReshadePresent);
			reshade::register_event<reshade::addon_event::bind_pipeline>(onBindPipeline);
			reshade::register_event<reshade::addon_event::draw>(onDraw);
			reshade::register_event<reshade::addon_event::draw_indexed>(onDrawIndexed);
			reshade::register_event<reshade::addon_event::draw_or_dispatch_indirect>(onDrawOrDispatchIndirect);
			reshade::register_event<reshade::addon_event::push_descriptors>(onPushDescriptors);
			reshade::register_overlay(nullptr, &displaySettings);
			loadShaderTogglerIniFile();
		}
		break;
	case DLL_PROCESS_DETACH:
		reshade::unregister_event<reshade::addon_event::reshade_present>(onReshadePresent);
		reshade::unregister_event<reshade::addon_event::destroy_pipeline>(onDestroyPipeline);
		reshade::unregister_event<reshade::addon_event::init_pipeline>(onInitPipeline);
		reshade::unregister_event<reshade::addon_event::reshade_overlay>(onReshadeOverlay);
		reshade::unregister_event<reshade::addon_event::bind_pipeline>(onBindPipeline);
		reshade::unregister_event<reshade::addon_event::draw>(onDraw);
		reshade::unregister_event<reshade::addon_event::draw_indexed>(onDrawIndexed);
		reshade::unregister_event<reshade::addon_event::draw_or_dispatch_indirect>(onDrawOrDispatchIndirect);
		reshade::unregister_event<reshade::addon_event::push_descriptors>(onPushDescriptors);
		reshade::unregister_event<reshade::addon_event::init_command_list>(onInitCommandList);
		reshade::unregister_event<reshade::addon_event::destroy_command_list>(onDestroyCommandList);
		reshade::unregister_event<reshade::addon_event::reset_command_list>(onResetCommandList);
		reshade::unregister_overlay(nullptr, &displaySettings);
		reshade::unregister_addon(hModule);
		break;
	}

	return TRUE;
}
