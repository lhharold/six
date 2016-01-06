#ifndef __SIX_STATICPLUGIJLOADER_H_INCLUDE__
#define __SIX_STATICPLUGIJLOADER_H_INCLUDE__

#include "core.h"
#if RENDER_SYS == RENDER_SYS_GL
#include "glplugin.h"
#elif RENDER_SYS == RENDER_SYS_GLES
#elif RENDER_SYS == RENDER_SYS_GLES2	
#elif RENDER_SYS == RENDER_SYS_DX9	
#endif

namespace six {
	class StaticPluginLoader {
	public:
    StaticPluginLoader() : mGLPlugin(NULL) {}
		void load();
		void unload();
	protected:
#if RENDER_SYS == RENDER_SYS_GL
		GLPlugin* mGLPlugin;
#elif RENDER_SYS == RENDER_SYS_GLES
#elif RENDER_SYS == RENDER_SYS_GLES2	
#elif RENDER_SYS == RENDER_SYS_DX9	
#endif
	};

	inline void StaticPluginLoader::load() {
		Root& root = Root::get();
#if RENDER_SYS == RENDER_SYS_GL
		mGLPlugin = NEW GLPlugin();
		root.installPlugin(mGLPlugin);
#elif RENDER_SYS == RENDER_SYS_GLES
#elif RENDER_SYS == RENDER_SYS_GLES2	
#elif RENDER_SYS == RENDER_SYS_DX9	
#endif
	}

	inline void StaticPluginLoader::unload() {
#if RENDER_SYS == RENDER_SYS_GL
		SAFE_DEL(mGLPlugin);
#elif RENDER_SYS == RENDER_SYS_GLES
#elif RENDER_SYS == RENDER_SYS_GLES2	
#elif RENDER_SYS == RENDER_SYS_DX9	
#endif
	}
}

#endif //__SIX_STATICPLUGIJLOADER_H_INCLUDE__