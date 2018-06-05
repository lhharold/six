#ifndef __SIX_THREADDEF_H_INCLUDE__
#define __SIX_THREADDEF_H_INCLUDE__

#define _AUTO_MUTEX_NAME mutex
#if SIX_THREAD_TYPE == 0
#define _AUTO_MUTEX
#define _LOCK_AUTO_MUTEX
#define _MUTEX(name)
#define _STATIC_MUTEX(name)
#define _STATIC_MUTEX_INSTANCE(name)
#define _LOCK_MUTEX(name)
#define _LOCK_MUTEX_NAMED(mutexName, lockName)
#define _AUTO_SHARED_MUTEX
#define _LOCK_AUTO_SHARED_MUTEX
#define _NEW_AUTO_SHARED_MUTEX
#define _DELETE_AUTO_SHARED_MUTEX
#define _COPY_AUTO_SHARED_MUTEX(from)
#define _SET_AUTO_SHARED_MUTEX_NULL
#define _MUTEX_CONDITIONAL(name) if(true)
#define _RW_MUTEX(name)
#define _LOCK_RW_MUTEX_READ(name)
#define _LOCK_RW_MUTEX_WRITE(name)
#define _THREAD_SYNCHRONISER(sync) 
#define _THREAD_WAIT(sync, lock) 
#define _THREAD_NOTIFY_ONE(sync) 
#define _THREAD_NOTIFY_ALL(sync) 
#define _THREAD_POINTER(T, var) T* var
#define _THREAD_POINTER_INIT(var) var(0)
#define _THREAD_POINTER_VAR(T, var) T* var = 0
#define _THREAD_POINTER_SET(var, expr) var = expr
#define _THREAD_POINTER_GET(var) var
#define _THREAD_POINTER_DELETE(var) { DELETE var; var = 0; }
#define _THREAD_SLEEP(ms)
#define _THREAD_WORKER_INHERIT
#elif _THREAD_TYPE == 1
#endif


#endif //__SIX_THREADDEF_H_INCLUDE__