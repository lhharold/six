#include "sixlog.h"

#if defined(_LOG4CPP)
#include <log4cpp/PropertyConfigurator.hh>
// appenders
#include <log4cpp/Appender.hh>
#include <log4cpp/OstreamAppender.hh>
#include <log4cpp/FileAppender.hh>
#include <log4cpp/RollingFileAppender.hh>
#include <log4cpp/AbortAppender.hh>
#ifdef WIN32
#include <log4cpp/Win32DebugAppender.hh>
#include <log4cpp/NTEventLogAppender.hh>
#endif
//#include <log4cpp/RemoteSyslogAppender.hh>
#ifdef LOG4CPP_HAVE_LIBIDSA
#include <log4cpp/IdsaAppender.hh>
#endif    // LOG4CPP_HAVE_LIBIDSA

#ifdef LOG4CPP_HAVE_SYSLOG
#include <log4cpp/SyslogAppender.hh>
#endif
// layouts
#include <log4cpp/Layout.hh>
#include <log4cpp/BasicLayout.hh>
#include <log4cpp/SimpleLayout.hh>
#include <log4cpp/PatternLayout.hh>
#include <log4cpp/Priority.hh>
#endif
#include "core.h"
namespace six {
  void Logexport::funInitializeLog4cpp(const char* logfile) {
#if defined(_LOG4CPP)
    try {
      log4cpp::PropertyConfigurator::configure(logfile);
    } catch (log4cpp::ConfigureFailure & f) {
      std::cerr << "Configure Problem " << f.what() << std::endl;

//#if defined(WIN32)
//  log4cpp::Appender * appender = new log4cpp::Win32DebugAppender("console");    
//#else
      log4cpp::Appender * appender = new log4cpp::OstreamAppender("console", &std::cout);
//#endif
      log4cpp::PatternLayout * patternLayout = new log4cpp::PatternLayout();
      patternLayout->setConversionPattern("%d [%t] %p - %m%n");
      appender->setLayout(patternLayout);
      log4cpp::Category & root = log4cpp::Category::getRoot();
      root.addAppender(appender);
      root.setPriority(log4cpp::Priority::DEBUG);
    }
#endif
  }
  void Logexport::funReleaseLog4cpp() {
#if defined(_LOG4CPP)
    log4cpp::Category::shutdown();
#endif
  }
#if defined(_LOG4CPP)
  log4cpp::Category& Logexport::GetLogCategory(const char * categoryName) {
    std::string name = "dk.";
    name.append(categoryName);
    return log4cpp::Category::getInstance(name);
  }
#endif
  void Logexport::suck(const char * fmt, ...) {
		static char buf[1024*8];
		va_list ap;
		va_start(ap, fmt);
		int r = vsnprintf(buf, sizeof(buf)-3, fmt, ap);
		va_end(ap);
		OutputDebugStringA(buf);
  }
}