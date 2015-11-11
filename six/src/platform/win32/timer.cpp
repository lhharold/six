#include "core.h"
#include "win32/timerimpl.h"

namespace six {
  Timer::Timer() : mTimerMask(0) {
    reset();
  }
  Timer::~Timer() {
  }
  void Timer::reset() {
    DWORD_PTR procMask;
    DWORD_PTR sysMask;
    GetProcessAffinityMask(GetCurrentProcess(), &procMask, &sysMask);

    if (procMask == 0)
      procMask = 1;

    if( mTimerMask == 0 ) {
      mTimerMask = 1;
      while((mTimerMask & procMask) == 0) {
        mTimerMask <<= 1;
      }
    }
    HANDLE thread = GetCurrentThread();
    DWORD_PTR oldMask = SetThreadAffinityMask(thread, mTimerMask);
    QueryPerformanceFrequency(&mFrequency);
    QueryPerformanceCounter(&mStartTime);
    mStartTick = GetTickCount();
    SetThreadAffinityMask(thread, oldMask);
    mLastTime = 0;
    mZeroClock = clock();
  }
  u64 Timer::getMilliseconds() {
    LARGE_INTEGER curTime;
    HANDLE thread = GetCurrentThread();
    DWORD_PTR oldMask = SetThreadAffinityMask(thread, mTimerMask);
    QueryPerformanceCounter(&curTime);
    SetThreadAffinityMask(thread, oldMask);

    LONGLONG newTime = curTime.QuadPart - mStartTime.QuadPart;
    u64 newTicks = (u64)(1000 * newTime / mFrequency.QuadPart);
    u64 check = GetTickCount() - mStartTick;
    s64 msecOff = (s64)(newTicks - check);
    if (msecOff < -100 || msecOff > 100) {
      LONGLONG adjust = MIN(msecOff * mFrequency.QuadPart / 1000, newTime - mLastTime);
      mStartTime.QuadPart += adjust;
      newTime -= adjust;
      newTicks = (u64)(1000 * newTime / mFrequency.QuadPart);
    }
    mLastTime = newTime;

    return newTicks;
  }
  u64 Timer::getMicroseconds() {
    LARGE_INTEGER curTime;
    HANDLE thread = GetCurrentThread();
    DWORD_PTR oldMask = SetThreadAffinityMask(thread, mTimerMask);
    QueryPerformanceCounter(&curTime);
    SetThreadAffinityMask(thread, oldMask);

    LONGLONG newTime = curTime.QuadPart - mStartTime.QuadPart;

    u64 newTicks = (u64) (1000 * newTime / mFrequency.QuadPart);
    u64 check = GetTickCount() - mStartTick;
    s64 msecOff = (s64)(newTicks - check);
    if (msecOff < -100 || msecOff > 100) {
      LONGLONG adjust = MIN(msecOff * mFrequency.QuadPart / 1000, newTime - mLastTime);
      mStartTime.QuadPart += adjust;
      newTime -= adjust;
    }

    mLastTime = newTime;
    return (u64)(1000000 * newTime / mFrequency.QuadPart);
  }
  u64 Timer::getMillisecondsCPU()
  {
    clock_t newClock = clock();
    return (u64)((f32)(newClock - mZeroClock) / ((float)CLOCKS_PER_SEC / 1000.0));
  }
  u64 Timer::getMicrosecondsCPU() {
    clock_t newClock = clock();
    return (u64)((f32)(newClock - mZeroClock) / ((f32)CLOCKS_PER_SEC / 1000000.f));
  }
}