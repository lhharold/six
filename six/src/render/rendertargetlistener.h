#ifndef __SIX_RENDERTARGETLISTENER_H_INCLUDE__
#define __SIX_RENDERTARGETLISTENER_H_INCLUDE__

namespace six {
  class RenderTarget;
  class Viewport;
	struct RenderTargetEvent {
		RenderTarget* source;
	};
	struct RenderTargetViewportEvent {
		Viewport* source;
	};
	class RenderTargetListener {
	public:
		virtual ~RenderTargetListener() {}
		virtual void preRenderTargetUpdate(const RenderTargetEvent& evt) {}
		virtual void postRenderTargetUpdate(const RenderTargetEvent& evt) {}
		virtual void preViewportUpdate(const RenderTargetViewportEvent& evt) {}
		virtual void postViewportUpdate(const RenderTargetViewportEvent& evt) {}
		virtual void viewportAdded(const RenderTargetViewportEvent& evt) {}
		virtual void viewportRemoved(const RenderTargetViewportEvent& evt) {}
	};
}

#endif //__SIX_RENDERTARGETLISTENER_H_INCLUDE__