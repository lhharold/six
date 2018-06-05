#ifndef __SIX_WIN32TIMERIMPL_H_INCLUDE__
#define __SIX_WIN32TIMERIMPL_H_INCLUDE__

namespace six {
  class Timer {
  public:
    Timer();
    ~Timer();
    u64 getMilliseconds();
    u64 getMicroseconds();
    u64 getMillisecondsCPU();
    u64 getMicrosecondsCPU();
    void reset();
  private:
    clock_t mZeroClock;

    DWORD mStartTick;
    LONGLONG mLastTime;
    LARGE_INTEGER mStartTime;
    LARGE_INTEGER mFrequency;

    DWORD_PTR mTimerMask;
  };
}
#endif //__SIX_TIMER_H_INCLUDE__
