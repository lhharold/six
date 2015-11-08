#include "glcore.h"
#include "glsupport.h"

#define GL_VERSION_HIGH		16
#define GL_VERSION_MID		8
#define GL_VERSION_LOW		0

namespace six {

	GLSupport::GLSupport() {

	}
	GLSupport::~GLSupport() {

	}

	void GLSupport::initialiseExtensions() {
		const GLubyte* ver = glGetString(GL_VERSION);
		ASSERT(ver && "get GL version error");
        String version = (const char*)ver;
		version = version.substr(0, version.find(" "));
		int first, second, third;
		SSCANF(version.c_str(), "%d.%d.%d", &first, &second, &third);
		mVersion |= first << GL_VERSION_HIGH;
		mVersion |= second << GL_VERSION_MID;
		mVersion |= third << GL_VERSION_LOW;

        const GLubyte* vendor = glGetString(GL_VENDOR);
        mVendor = (const char*)vendor;

        const char* renderer = (const char*)glGetString(GL_RENDERER);

		const GLubyte* ext = glGetString(GL_EXTENSIONS);
        ASSERT(ext && "get GL extension error");
        mExtensions = (const char*)ext;
	}

    bool GLSupport::checkMinGLVersion(const char* v) const {
		int first, second, third;
		SSCANF(v, "%d.%d.%d", &first, &second, &third);
		u32 version = 0;
		version |= first << GL_VERSION_HIGH;
		version |= second << GL_VERSION_MID;
		version |= third << GL_VERSION_LOW;
		return version < mVersion;
    }

	bool GLSupport::checkExtension(const char* ext) const {
		return STRSTR(mExtensions.c_str(), ext) != NULL;
	}
}