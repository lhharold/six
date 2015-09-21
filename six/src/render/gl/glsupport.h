#ifndef __SIX_GLSUPPORT_H__INCLUDE
#define __SIX_GLSUPPORT_H__INCLUDE

namespace six {
	class GLRenderWindow;
	class GLRenderSystem;
	class GLSupport {
	public:
		enum SUPPORT_VENDOR{
			VENDOR_UNKONW = 0,
			VENDOR_NVIDIA,
			VENDOR_ATI,
			VENDOR_AMD,
			VENDOR_Intel,
			VENDOR_S3,
			VENDOR_Matrox,
			VENDOR_3DLabs,
			VENDOR_SiS,
		};

		GLSupport();
		~GLSupport();

		virtual void initialiseExtensions();
		virtual GLRenderWindow* createWindow(GLRenderSystem* renderSystem, bool autoWindow, const char* windowName) = 0;
		virtual GLRenderWindow* newWindow(const char* windowName, u32 width, u32 height, bool fullScreen) = 0;

		virtual void startup() = 0;

	    bool checkMinGLVersion(const char* v) const;
		bool checkExtension(const char* ext) const;
	protected:
		u32 mVersion;
		const char* mVendor;
		String mExtensions;
	};
}

#endif //__SIX_GLSUPPORT_H__INCLUDE