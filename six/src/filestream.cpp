#include "define.h"
#include "filestream.h"

namespace six {

	FileStream::FileStream(const char* name) 
		: IDataStream(name)
		, mFile(NULL)
		, mError(FILE_ERROR_NO_ERROR)
	{
	}

	bool FileStream::open(const char* flags) {
		if(mFile != NULL) {
			mError = FILE_ERROR_OPENED;
			return false;
		}
		mFile = FOPEN(mName.c_str(), flags);
		if(mFile == NULL) {
			mError = FILE_ERROR_FAILD_OPEN;
			return false;
		}

		FSEEK(mFile, 0, SEEK_END);
		mSize = FTELL(mFile);
		if(mSize < 0) {
			mError = FILE_ERROR_LENGTH_ERROR;
			return false;
		}
		FSEEK(mFile, 0, SEEK_SET);
		return true;
	}

	bool FileStream::close() {
		if (mFile != NULL) {
			FCLOSE(mFile);
			mFile = NULL;
			mSize = 0;
			(*operators._close_ptr)(fp, operators._user_data);
			fp	   = NULL;
			length = -1;
			origin = 0;

			defaultOperators();
		}
	}

	u32 FileStream::read(void* buff, u32 size) {
		
	}
		u32 FileStream::write(void* buff, u32 size) {return 0;}
		u32 FileStream::readLine(char* buff, u32 maxCount, const char* delim = "\n");
		u32 FileStream::skipLine(const char* delim = "\n");
		void FileStream::skip(s32 count);
		void FileStream::seek(u32 pos);
		u32 FileStream::tell();
		bool FileStream::eof();

}