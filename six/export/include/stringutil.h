#ifndef __SIX_STRING_H__
#define __SIX_STRING_H__

namespace six {

	template<typename T>
	class Vector : public std::vector<T> {};
	template<typename K, typename T>
	class Map : public std::map<K, T> {};

	class StringUtil {
	public:
        static void trim(String& str, bool left = true, bool right = true);

		static u32 split(Vector<String>& out, const String str, const String& delims = "\t\n ", u32 maxSplits = 0, bool preserveDelims = false);

		static u32 tokenise(Vector<String>&out, const String& str, const String& singleDelims = "\t\n ", const String& doubleDelims = "\"", unsigned int maxSplits = 0);

        static void toLowerCase(String& str);

        static void toUpperCase(String& str);

	    static bool startsWith(const String& str, const String& pattern, bool lowerCase = true);

        static bool endsWith(const String& str, const String& pattern, bool lowerCase = true);

        static String standardisePath( const String &init, bool appendEndSeparator = true);

		static String normalizeFilePath(const char* init, bool makeLowerCase = true);

        static void splitFilename(const String& qualifiedName, String& outBasename, String& outPath);

		static void splitBaseFilename(const String& fullName, String& outBasename, String& outExtention);

		static void splitFullFilename(const String& qualifiedName, String& outBasename, String& outExtention, String& outPath);

        static bool match(const String& str, const String& pattern, bool caseSensitive = true);
		
		static String replaceAll(const String& source, const String& replaceWhat, const String& replaceWithWhat);

		static const String BLANK;
	};
}

#endif //