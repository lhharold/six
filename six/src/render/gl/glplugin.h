#ifndef __SIX_GLPLUGIN_H_INCLUDE__
#define __SIX_GLPLUGIN_H_INCLUDE__

namespace six {
  class GLRenderSystem;
	class GLPlugin : public Plugin {
	public:
		GLPlugin();
		virtual const char* getName() const;
		virtual void install();
		virtual void initialise();
		virtual void shutdown();
		virtual void uninstall();

	protected:
		GLRenderSystem* mRenderSystem;

	};
}
#endif