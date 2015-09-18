#include "six.h"
using namespace six;

int main(int argn, const char* argv[]) {

	FileSystem fileSystem(".");
	IDataStream* vsStream = fileSystem.open("default_vs.glsl");
	IDataStream* psStream = fileSystem.open("default_ps.glsl");

	char str[512];
	memset(str, 0, sizeof(str));
	printf("%%%%%%%%%%%%vs%%%%%%%%%%%%%%%%%\n");
	vsStream->readBuffer(str, sizeof(str));
	printf("%s\n", str);
	memset(str, 0, sizeof(str));
	psStream->readBuffer(str, sizeof(str));
	printf("%%%%%%%%%%%%ps%%%%%%%%%%%%%%%%%\n");
	printf("%s\n", str);

	SAFE_DEL(vsStream);
	SAFE_DEL(psStream);

	return 0;
}
