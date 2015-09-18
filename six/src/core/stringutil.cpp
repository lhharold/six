#include "define.h"
#include "stringutil.h"

namespace six {

	const String StringUtil::BLANK = "";

	void StringUtil::trim(String& str, bool left, bool right) {
        static const String delims = " \t\r";
        if(right)
            str.erase(str.find_last_not_of(delims)+1); // trim right
        if(left)
            str.erase(0, str.find_first_not_of(delims)); // trim left
	}

	u32 StringUtil::split(Vector<String>& out, const String str, const String& delims, u32 maxSplits, bool preserveDelims) {
        out.reserve(maxSplits ? maxSplits+1 : 10);
        u32 numSplits = 0;
        u32 start = 0, pos;
        do {
            pos = str.find_first_of(delims, start);
            if (pos == start) {
                start = pos + 1;
            } else if (pos == String::npos || (maxSplits && numSplits == maxSplits)) {
                out.push_back(str.substr(start));
                break;
            } else {
                out.push_back(str.substr(start, pos - start));
                if(preserveDelims) {
                    u32 delimStart = pos, delimPos;
                    delimPos = str.find_first_not_of(delims, delimStart);
                    if (delimPos == String::npos) {
                        out.push_back( str.substr(delimStart) );
                    } else {
                        out.push_back( str.substr(delimStart, delimPos - delimStart) );
                    }
                }
                start = pos + 1;
            }
            // parse up to next real data
            start = str.find_first_not_of(delims, start);
            ++numSplits;
        } while (pos != String::npos);
		return out.size();
	}

	u32 StringUtil::tokenise(Vector<String>&out, const String& str, const String& singleDelims, const String& doubleDelims, unsigned int maxSplits) {
        // Pre-allocate some space for performance
        out.reserve(maxSplits ? maxSplits+1 : 10);    // 10 is guessed capacity for most case

        u32 numSplits = 0;
		String delims = singleDelims + doubleDelims;

        u32 start = 0, pos;
		char curDoubleDelim = 0;
        start = 0;
        do {
			if (curDoubleDelim != 0) {
				pos = str.find(curDoubleDelim, start);
			} else {
				pos = str.find_first_of(delims, start);
			}
            if (pos == start) {
				char curDelim = str.at(pos);
				if (doubleDelims.find_first_of(curDelim) != String::npos) {
					curDoubleDelim = curDelim;
				}
                start = pos + 1;
            } else if (pos == String::npos || (maxSplits && numSplits == maxSplits)) {
				if (curDoubleDelim != 0) {
					ASSERT(0 && "StringUtil::tokenise - Missing closer doubleDelim.");
				}
                out.push_back( str.substr(start) );
                break;
            } else {
				if (curDoubleDelim != 0) {
					curDoubleDelim = 0;
				}
				out.push_back( str.substr(start, pos - start) );
				start = pos + 1;
            }
			if (curDoubleDelim == 0) {
				start = str.find_first_not_of(singleDelims, start);
			}
            ++numSplits;
        } while (start != String::npos);
		return out.size();
	}

	void StringUtil::toLowerCase(String& str) {
        std::transform( str.begin(), str.end(), str.begin(), tolower);
	}

	void StringUtil::toUpperCase(String& str) {
        std::transform( str.begin(), str.end(), str.begin(), toupper);
	}

    bool StringUtil::startsWith(const String& str, const String& pattern, bool lowerCase) {
        u32 thisLen = str.length();
        u32 patternLen = pattern.length();
        if (thisLen < patternLen || patternLen == 0)
            return false;
        String startOfThis = str.substr(0, patternLen);
        if (lowerCase)
            StringUtil::toLowerCase(startOfThis);
        return (startOfThis == pattern);
    }

	bool StringUtil::endsWith(const String& str, const String& pattern, bool lowerCase) {
		u32 thisLen = str.length();
		u32 patternLen = pattern.length();
		if (thisLen < patternLen || patternLen == 0)
			return false;
		String endOfThis = str.substr(thisLen - patternLen, patternLen);
		if (lowerCase)
			StringUtil::toLowerCase(endOfThis);
		return (endOfThis == pattern);
	}

	String StringUtil::standardisePath(const String &init, bool appendEndSeparator) {
        String path = init;
        std::replace(path.begin(), path.end(), '\\', '/');
        if(appendEndSeparator && path[path.length() - 1] != '/')
            path += '/';
        return path;
	}

	String StringUtil::normalizeFilePath(const char* init, bool makeLowerCase) {
		u32 pathLen = STRLEN(init);

		u32 indexSrc = 0;
		int indexDst = 0;
		int metaPathArea = 0;

		char reservedBuf[1024];
		char* bufferDst = reservedBuf;
		bool isDestAllocated = false;

		if (pathLen > 1023) {
			isDestAllocated = true;
			bufferDst = NEW char[pathLen + 1];
		}

		while (indexSrc < pathLen) {		
			if ((init[indexSrc] == '\\') || (init[indexSrc] == '/')) {
				++indexSrc;
				continue;
			} else {
				if ((init[indexSrc] == '.') && ((init[indexSrc + 1] == '\\') || (init[indexSrc + 1] == '/'))) {
					indexSrc += 2;
					continue;
				} else if ((init[indexSrc] == '.') && (init[indexSrc + 1] == '.') && ((init[indexSrc + 2] == '\\') || (init[indexSrc + 2] == '/'))) {
					if (indexDst > metaPathArea) {
						do {
							--indexDst;
						} while ((indexDst > metaPathArea) && (bufferDst[indexDst - 1] != '/'));
						indexSrc += 3;
						continue;
					} else {
						metaPathArea += 3;
					}
				}
			}
			while (indexSrc < pathLen) {
				char curChar = init[indexSrc];
				if (makeLowerCase) {
					curChar = tolower(curChar);
				}
				if ((curChar == '\\')) {
					curChar = '/';
				}
				bufferDst[indexDst] = curChar;
				++indexDst;
				++indexSrc;
				if (curChar == '/') break;
			}
		}
		bufferDst[indexDst] = 0;

		String normalized(bufferDst); 
		if (isDestAllocated) {
			delete[] bufferDst;
		}
		return normalized;		
	}

	//C:/1.txt or /home/1.txt
	void StringUtil::splitFilename(const String& qualifiedName, String& outBasename, String& outPath) {
		String path = qualifiedName;
		std::replace( path.begin(), path.end(), '\\', '/' );
		u32 i = path.find_last_of('/');

		if (i == String::npos) {
			outPath.clear();
			outBasename = qualifiedName;
		} else {
			outBasename = path.substr(i+1, path.size() - i - 1);
			outPath = path.substr(0, i+1);
		}
	}

	//1.txt
	void StringUtil::splitBaseFilename(const String& fullName, String& outBasename, String& outExtention) {
		u32 i = fullName.find_last_of(".");
		if (i == String::npos) {
			outExtention.clear();
			outBasename = fullName;
		} else {
			outExtention = fullName.substr(i+1);
			outBasename = fullName.substr(0, i);
		}
	}

	void StringUtil::splitFullFilename(const String& qualifiedName, String& outBasename, String& outExtention, String& outPath) {
		String fullName;
		splitFilename(qualifiedName, fullName, outPath);
		splitBaseFilename(fullName, outBasename, outExtention);
	}

	bool StringUtil::match(const String& str, const String& pattern, bool caseSensitive) {
		String tmpStr = str;
		String tmpPattern = pattern;
		if (!caseSensitive) {
			StringUtil::toLowerCase(tmpStr);
			StringUtil::toLowerCase(tmpPattern);
		}

		String::const_iterator strIt = tmpStr.begin();
		String::const_iterator patIt = tmpPattern.begin();
		String::const_iterator lastWildCardIt = tmpPattern.end();
		while (strIt != tmpStr.end() && patIt != tmpPattern.end()) {
			if (*patIt == '*') {
				lastWildCardIt = patIt;
				++patIt;
				if (patIt == tmpPattern.end()) {
					strIt = tmpStr.end();
				} else {
					while(strIt != tmpStr.end() && *strIt != *patIt) {
						++strIt;
					}
				}
			} else {
				if (*patIt != *strIt) {
					if (lastWildCardIt != tmpPattern.end()) {
						patIt = lastWildCardIt;
						lastWildCardIt = tmpPattern.end();
					} else {
						return false;
					}
				} else {
					++patIt;
					++strIt;
				}
			}
		}
		if (patIt == tmpPattern.end() && strIt == tmpStr.end()) {
			return true;
		}
		return false;
	}

	String StringUtil::replaceAll(const String& source, const String& replaceWhat, const String& replaceWithWhat) {
		String result = source;
        String::size_type pos = 0;

		u32 replaceWhatLen = replaceWhat.size();
		while(1) {
			pos = result.find(replaceWhat, pos);
			if (pos == String::npos) {
				break;
			}
			result.replace(pos, replaceWhatLen, replaceWithWhat);
            pos += replaceWhatLen;
		}
		return result;
	}
}