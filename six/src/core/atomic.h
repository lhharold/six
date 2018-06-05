#ifndef __SIX_ATOMIC_H_INCLUDE__
#define __SIX_ATOMIC_H_INCLUDE__

namespace six {
  template<typename T>
  class Atomic {
  public:
    Atomic(const T& val) :mValue(val) {}
    Atomic(const Atomic<T>& cousin) : mValue(cousin.mValue) {}
    Atomic() {}
    T get() const {return mValue;}
    void set(const T& val) {
      _LOCK_AUTO_MUTEX;
      mValue = val;
    }
    bool cas(const T& oldVal, const T& newVal) {
      _LOCK_AUTO_MUTEX;
      if (mValue != oldVal)
        return false;
      mValue = newVal;
      return true;
    }
    T operator++ (void) {
      _LOCK_AUTO_MUTEX;
      return ++mValue;
    }
    T operator-- (void) {
      _LOCK_AUTO_MUTEX;
      return --mValue;
    }
    T operator++ (int) {
      _LOCK_AUTO_MUTEX;
      return mValue++;
    }
    T operator-- (int) {
      _LOCK_AUTO_MUTEX;
      return mValue--;
    }
  protected:
    _AUTO_MUTEX;
    volatile T mValue;
  };
}

#endif //__SIX_ATOMIC_H_INCLUDE__