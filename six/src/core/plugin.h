#ifndef __SIX_PLUGIN_H_INCLUDE__
#define __SIX_PLUGIN_H_INCLUDE__

namespace six {
	class Plugin {
	public:
		Plugin() {}
		virtual ~Plugin() {}

		virtual const char* getName() const = 0;
		virtual void install() = 0;
		virtual void initialise() = 0;
		virtual void shutdown() = 0;
		virtual void uninstall() = 0;
	};
}
#endif //__SIX_PLUGIN_H_INCLUDE__