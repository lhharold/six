#ifndef __SIX_FRAMELISTENER_H_INCLUDE__
#define __SIX_FRAMELISTENER_H_INCLUDE__

namespace six {

  struct FrameEvent {
    f32 timeSinceLastEvent;
    f32 timeSinceLastFrame;
  };

	class FrameListener {
	public:
		FrameListener();
		virtual ~FrameListener();

    virtual bool frameStart(const FrameEvent& evt) {return true;}
    virtual bool frameRender(const FrameEvent& evt) {return true;}
    virtual bool frameEnd(const FrameEvent& evt) {return true;}
	protected:
	};
}
#endif //__SIX_FRAMELISTENER_H_INCLUDE__