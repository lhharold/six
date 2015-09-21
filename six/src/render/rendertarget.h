#ifndef __SIX_RENDERTARGET_H_INCLUDE__
#define __SIX_RENDERTARGET_H_INCLUDE__

namespace six {

	class RenderTarget {
	public:
		RenderTarget(const char* name) {}
		virtual ~RenderTarget() {}

		const char* getName() const {return mName.c_str();}
	protected:
		String mName;
	};

}
#endif //__SIX_RENDERTARGET_H_INCLUDE__