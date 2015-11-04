#ifndef __SIX_COLOR_H_INCLUDE__
#define __SIX_COLOR_H_INCLUDE__

namespace six {
	class Color4i {
	public:
		union {
			struct {
				u8 r;
				u8 g;
				u8 b;
				u8 a;
			};
			u8  rgba[4];
			u32 value;
		};

		Color4i() : value(0) {}
		Color4i(const Color4i& ref) {value = ref.value;}
		Color4i(int red, int green, int blue, int alpha = 255) {r = CLAMP(red, 0, 255); g = CLAMP(green, 0, 255); b = CLAMP(blue, 0, 255); a = CLAMP(alpha, 0, 255);}
		Color4i& set(int red, int green, int blue, int alpha = 255) {r = CLAMP(red, 0, 255); g = CLAMP(green, 0, 255); b = CLAMP(blue, 0, 255); a = CLAMP(alpha, 0, 255); return *this;}

		const Color4i& operator = (const Color4i& clr) {value = clr.value; return *this;}
		bool operator == (const Color4i& clr) const {return value == clr.value;}
		bool operator != (const Color4i& clr) const {return value != clr.value;}
		u8 operator[] (u8 i) {ASSERT(i >= 0 && i < 4); return rgba[i];}
		int compare(const Color4i& clr) {return value  == clr.value ? (0 : value < clr.value ? -1 : 1);}
		Color4i operator + (const Color4i& clr) const{u8 maxV = 255; return Color4i(MIN(maxV, r+clr.r), MIN(maxV, g+clr.g), MIN(maxV, b+clr.b), MIN(maxV, a+clr.a));}
		Color4i& operator += (const Color4i& clr) {return (*this = *this+clr);}
		Color4i operator - (const Color4i& clr) const{u8 minV = 0; return Color4i(MAX(minV, r-clr.r), MAX(minV, g-clr.g), MIN(minV, b-clr.b), MIN(minV, a-clr.a));}
		Color4i& operator -= (const Color4i& clr) {return (*this = *this-clr);}
		Color4i operator * (f32 val)const {return Color4i((u8)(CLAMP(F_ROUND(r*val), 0, 255)), (u8)(CLAMP(F_ROUND(g*val), 0, 255)), (u8)(CLAMP(F_ROUND(b*val), 0, 255)), (u8)(CLAMP(F_ROUND(a*val), 0, 255)));}
		Color4i& operator *= (f32 val) {return (*this = *this*val);}
	};
	class Color4f {
	public:
		static const Color4f Zero = Color4f();
		static const Color4f Black = Color4f(0.f, 0.f, 0.f);
		static const Color4f White = Color4f(1.f, 1.f, 1.f);
		static const Color4f Red = Color4f(1.f, 0.f, 0.f);
		static const Color4f Green = Color4f(0.f, 1.f, 0.f);
		static const Color4f Blue = Color4f(0.f, 0.f, 1.f);
		union {
			struct {
				f32 r;
				f32 g;
				f32 b;
				f32 a;
			};
			f32 value[4];
		};
		Color4f() {MEMSET(this, 0, sizeof(Color4f));}
		Color4f(f32 red, f32 green, f32 blue, f32 alpha = 1.f) {r = CLAMP(red, 0.f, 1.f); g = CLAMP(green, 0.f, 1.f); b = CLAMP(blue, 0.f, 1.f); alpha = CLAMP(alpha, 0.f, 1.f);}
		Color4f(const Color4f& clr) {*this = clr;}
		Color4f& set(f32 red, f32 green, f32 blue, f32 alpha = 1.f) {r = CLAMP(red, 0.f, 1.f); g = CLAMP(green, 0.f, 1.f); b = CLAMP(blue, 0.f, 1.f); alpha = CLAMP(alpha, 0.f, 1.f); return *this;}

		const Color4f& operator = (const Color4f& clr) {MEMCPY(value, clr.value, sizeof(value)); return *this;}
		bool operator == (const Color4f& clr) const {return F_EQUAL(r, clr.r) && F_EQUAL(g, clr.g) && F_EQUAL(b, clr.b) && F_EQUAL(a, clr.a);}
		bool operator != (const Color4f& clr) const {return !(operator == (clr));}
		f32 operator[] (u8 i) {ASSERT(i >= 0 && i < 4); return value[i];}
		Color4f operator + (const Color4f& clr) const{return Color4f(r+clr.r, g+clr.g, b+clr.b, a+clr.a);}
		Color4f& operator += (const Color4f& clr) {return (*this = *this+clr);}
		Color4f operator - (const Color4f& clr) const{return Color4f(r-clr.r, g-clr.g, b-clr.b, a-clr.a);}
		Color4f& operator -= (const Color4f& clr) {return (*this = *this-clr);}
		Color4f operator * (f32 val)const {return Color4f(r*val, g*val, b*val, a*val);}
		Color4f& operator *= (f32 val) {return (*this = *this*val);}
		Color4f operator * (const Color4f& clr)const {return Color4f(r*clr.r, g*clr.g, b*clr.b, a*clr.a);}
		Color4f& operator *= (const Color4f& clr) {return (*this = *this*clr);}
	};
	typedef Color4f Color;
}

#endif //__SIX_COLOR_H_INCLUDE__