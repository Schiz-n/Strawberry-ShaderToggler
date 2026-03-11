// WIC-based PNG loader, isolated in its own translation unit to avoid
// name conflicts between <wincodec.h> and the reshade::api namespace.
#include <windows.h>
#include <wincodec.h>
#pragma comment(lib, "windowscodecs.lib")

#include <vector>
#include <filesystem>
#include <cstdint>

bool loadPngRgba(const std::filesystem::path& path, std::vector<uint8_t>& pixels, uint32_t& w, uint32_t& h)
{
	IWICImagingFactory* factory = nullptr;
	if (FAILED(CoCreateInstance(CLSID_WICImagingFactory, nullptr, CLSCTX_INPROC_SERVER,
	                            __uuidof(IWICImagingFactory), reinterpret_cast<void**>(&factory))))
		return false;

	bool ok = false;
	IWICBitmapDecoder* decoder = nullptr;
	if (SUCCEEDED(factory->CreateDecoderFromFilename(path.wstring().c_str(), nullptr,
	              GENERIC_READ, WICDecodeMetadataCacheOnLoad, &decoder)))
	{
		IWICBitmapFrameDecode* frame = nullptr;
		if (SUCCEEDED(decoder->GetFrame(0, &frame)))
		{
			IWICFormatConverter* conv = nullptr;
			if (SUCCEEDED(factory->CreateFormatConverter(&conv)) &&
			    SUCCEEDED(conv->Initialize(frame, GUID_WICPixelFormat32bppRGBA,
			              WICBitmapDitherTypeNone, nullptr, 0.0, WICBitmapPaletteTypeCustom)))
			{
				UINT fw = 0, fh = 0;
				conv->GetSize(&fw, &fh);
				w = fw; h = fh;
				pixels.resize(static_cast<size_t>(w) * h * 4);
				ok = SUCCEEDED(conv->CopyPixels(nullptr, w * 4, static_cast<UINT>(pixels.size()), pixels.data()));
			}
			if (conv) conv->Release();
			frame->Release();
		}
		decoder->Release();
	}
	factory->Release();
	return ok;
}
