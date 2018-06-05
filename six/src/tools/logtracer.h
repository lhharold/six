#ifndef __SIX_LOGTRACER_H_INCLUDE__
#define __SIX_LOGTRACER_H_INCLUDE__

#include "config.h"
#include <string>

#if defined(_LOG4CPP) && defined(WIN32)
#include <stdlib.h>
#include "log4cpp/Category.hh"
namespace six {
  class LogTracer {
  private:
    LogTracer(const LogTracer &);
    LogTracer& operator = (const LogTracer &);
  public:
    LogTracer(log4cpp::Category& log, const char* prefix) : mLog(log) , mMsg(prefix) {}
    void operator() (const char* fmt, ...) {
      va_list ap;
      va_start(ap, fmt);
      _appendString(fmt, ap);
      va_end(ap);
      _writeLog(mMsg.c_str());
    }
  protected:
    virtual void _writeLog(const char* message) = 0;
    void _appendString(const char* format, const va_list& args) {
      int size = 1024;
      char* buffer = (char*)malloc(size);
      while(true) {
        int n = _vsnprintf(buffer, size, format, args);
        if((n > -1) && (n < size)) {
          mMsg.append(buffer);
          break;
        }

        size = (n > -1) ? n + 1 : size * 2;
        buffer = (char*)realloc(buffer, size);
      }
      free(buffer);
    }
  protected:
    log4cpp::Category& mLog;
    std::string mMsg;
  };

  class LogDebuger : public LogTracer {
  public:
    LogDebuger(log4cpp::Category& log, const char* prefix) : LogTracer(log, prefix) {}
  protected:
    virtual void _writeLog(const char* msg) {
      mLog.debug(msg);
    }
  };

  class LogInfoer : public LogTracer {
  public:
    LogInfoer(log4cpp::Category& log, const char* prefix) : LogTracer(log, prefix) {}
  protected:
    virtual void _writeLog(const char* msg) {
      mLog.info(msg);
    }
  };
  class LogNoticer : public LogTracer {
  public:
    LogNoticer(log4cpp::Category& log, const char* prefix) : LogTracer(log, prefix) {}
  protected:
    virtual void _writeLog(const char* msg) {
      mLog.notice(msg);
    }
  };
  class LogErrorer : public LogTracer {
  public:
    LogErrorer(log4cpp::Category& log, const char* prefix) : LogTracer(log, prefix) {}
  protected:
    virtual void _writeLog(const char* msg) {
      mLog.error(msg);
    }
  };
}
#endif
#endif //__SIX_LOGTRACER_H_INCLUDE__