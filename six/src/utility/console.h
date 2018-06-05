#ifndef __SIX_CONSOLE_H_INCLUDE__
#define __SIX_CONSOLE_H_INCLUDE__

namespace six {
	class Console {
	public:
		Console();
		virtual ~Console();
	protected:
		void* mConsole;
	};
}
#endif //__SIX_CONSOLE_H_INCLUDE__