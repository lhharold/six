#ifndef __SIX_UTILITY_H_INCLUDED_
#define __SIX_UTILITY_H_INCLUDED_
#include <math.h>
#include "_math.h"
namespace six {
  namespace util {
		inline void normalizeVector(f32 vec[3]) {
      f32 invLen = Math::invsqrt(vec[0]*vec[0]+vec[1]*vec[1]+vec[2]*vec[2]);
			vec[0] *= invLen;
			vec[1] *= invLen;
			vec[2] *= invLen;
		}
		inline f32 dotVector(const f32 a[3], const f32 b[3]) {
			return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
		}
		inline void crossVector(const f32 a[3], const f32 b[3], f32 out[3]) {
			out[0] = -a[1]*b[2] + a[2]*b[1];
			out[1] = -a[2]*b[0] + a[0]*b[2];
			out[2] = -a[0]*b[1] + a[1]*b[0];
		}
		inline void getPlane(f32 a[3], f32 b[3], f32 c[3], f32* plane_d, f32 plane_n[3]) {
			f32 ab[3] = {b[0]-a[0], b[1]-a[1], b[2]-a[2]};
			f32 ac[3] = {c[0]-a[0], c[1]-a[1], c[2]-a[2]};
			crossVector(ab, ac, plane_n);
			normalizeVector(plane_n);
			*plane_d = -dotVector(a, plane_n);
		}
		inline int pointRelationPlane(const f32 pt[3], const f32 plane_n[3], f32 plane_d) {
			f32 m = dotVector(pt, plane_n) + plane_d;
      if(F_LESS_ZERO(m)) return -1;
			if(F_MORE_ZERO(m)) return 1;
			return 0;
		}
		inline bool isFrontFacingPlane(f32* plane_n, f32* dir) {
			const f32 m = dotVector(plane_n, dir);
			return F_LESS_ZERO(m);
		}
		inline f32 getDistToPlane(f32* pt, f32* plane_n, f32 plane_d) {
			return dotVector(pt, plane_n) + plane_d;
		}
		inline bool getLowestRoot(f32 a, f32 b, f32 c, f32 maxR, f32* root) {
			f32 determinant = b*b - 4.f*a*c;
			if(determinant < 0.0f) 
				return false;

			f32 sqrtD = (f32)sqrt(determinant);
			f32 r1 = (-b - sqrtD) / (2*a);
			f32 r2 = (-b + sqrtD) / (2*a);
			if(r1 > r2) {
        f32 tmp=r2; r2=r1; r1=tmp;
      }

			if(r1 > 0 && r1 < maxR) {
				*root = r1;
				return true;
			}

			// its possible that we want x2, this can happen if x1 < 0
			if(r2 > 0 && r2 < maxR) {
				*root = r2;
				return true;
			}
			return false;
		}
		inline bool lineIntersectWith(f32 l0s[2], f32 l0e[2], f32 l1s[2], f32 l1e[2], f32* out = NULL, f32* _t = NULL) {
			f32 d0[2] = {l0e[0]-l0s[0], l0e[1]-l0s[1]};
			f32 d1[2] = {l1s[0]-l1e[0], l1s[1]-l1e[1]};

			// check "parallel" lines
			f32 det = d0[0]*d1[1] - d0[1]*d1[0];
			if(F_ZERO(det))
				return false;
			det = 1.0f/det;
			f32 d01[2] = {l1s[0]-l0s[0], l1s[1]-l0s[1]};
			// Check intersection with this line
			//f32 t = (d01.X * d1.Y - d01.Y * d1.X) * det;
			f32 t = (d01[0]*d1[1] - d01[1]*d1[0]) * det;
			if(t < 0.0f || t > 1.0f)
				return false;
			// check intersection with other line
			t = (d0[0]*d01[1] - d0[1]*d01[0]) * det;
			if(t < 0.0f || t > 1.0f)
				return false;
			if(out)  {
				out[0] = l1s[0] - d1[0]*t;
				out[1] = l1s[1] - d1[1]*t;
			}
			if(_t) *_t = t;
			return true;
		}

		inline bool lineIntersectWithCircle(const f32 ls[2], const f32 le[2], f32 maxLambda, const f32 co[2], const f32 cr, f32*lambda, f32 normal[2]) {
			f32 s[2] = {ls[0]-co[0], ls[1]-co[1]};
			f32 b = s[0]*s[0]+s[1]*s[1] - cr*cr;
			// Does the segment start inside the circle?
			if(b < 0.0f)
				return false;

			// Solve quadratic equation.
			f32 r[2] = {le[0]-ls[0], le[1]-ls[1]};
			f32 c  = s[0]*r[0] + s[1]*r[1];
			f32 rr = r[0]*r[0] + r[1]*r[1];
			f32 sigma = c*c - rr*b;

			// Check for negative discriminant and short segment.
			if(sigma < 0.0f || rr < F32_MIN_VALUE)
				return false;

			// Find the point of intersection of the line with the circle.
      f32 a = -(c + Math::sqrt(sigma));

			// Is the intersection point on the segment?
			if(0.f <= a && a <= maxLambda * rr) {
				a /= rr;
				if(lambda) *lambda = a;
				if(normal) {
					normal[0] = s[0] + a*r[0];
					normal[1] = s[1] + a*r[1];
					
					f32 nl = Math::invsqrt(normal[0]*normal[0] + normal[1]*normal[1]);
					normal[0] *= nl;
					normal[1] *= nl;
				}
				return true;
			}
			return false;
		}

		inline bool getTriFactor(const f32 n[3], const f32 a[3], const f32 b[3], const f32 c[3], const f32 pt[3], f32 out[3], f32 precision = F_PRECISION) {
			f32 edge1[3] = {b[0]-a[0], b[1]-a[1], b[2]-a[2]};
			f32 edge2[3] = {c[0]-a[0], c[1]-a[1], c[2]-a[2]};

			f32 pvec[3];
			crossVector(n, edge2, pvec);

			f32 det = dotVector(edge1, pvec);
			f32 inv_det = 1.f/det;

			f32 tvec[3] = {pt[0]-a[0], pt[1]-a[1], pt[2]-a[2]};
			f32 qvec[3];
			crossVector(tvec, edge1, qvec);
			if(F_ZERO(det, precision))
				return false;

			out[1] = dotVector(tvec, pvec);
			if(F_ZERO(out[1], precision))
				out[1] = 0.f;

			if(F_EQUAL(out[1], det, precision))
				out[1] = det;

			if(det > 0.f) {
				if(F_LESS_ZERO(out[1]) || F_MORE_ZERO(out[1]-det))
					return false;
			} else {
				if(F_MORE_ZERO(out[1]) || F_LESS_ZERO(out[1]-det))
					return false;
			}

			out[2] = dotVector(n, qvec);
			if(F_ZERO(out[2], precision))
				out[2] = 0.f;

			if(F_EQUAL(out[2], det, precision))
				out[2] = det;

			if(F_EQUAL(out[1]+out[2], det, precision))
				out[2] = det - out[1];

			if(det > 0.f) {
				if(F_LESS_ZERO(out[2]) || F_MORE_ZERO(out[1]+out[2]-det))
					return false;
			} else {
				if(F_MORE_ZERO(out[2]) || F_LESS_ZERO(out[1]+out[2]-det))
					return false;
			}

			out[1] *= inv_det;
			out[2] *= inv_det;
			out[0] = 1.f - out[1] - out[2];
			return true;
		}
 
		template<typename T>
		inline bool getTriFactor2d(T* a, T* b, T* c, T* pt, T* out, T precision = F_PRECISION) {
			T edge1[2] = {b[0]-a[0], b[1]-a[1]};
			T edge2[2] = {c[0]-a[0], c[1]-a[1]};

			T pvec[2] = {edge2[1], -edge2[0]};

			T det		= edge1[0]*pvec[0] + edge1[1]*pvec[1];
			T inv_det = 1.f/det;

			T tvec[2] = {pt[0]-a[0], pt[1]-a[1]};
			T qvec = -tvec[0]*edge1[1] + tvec[1]*edge1[0];

			if(F_ZERO(det, precision))
				return false;

			out[1] = tvec[0]*pvec[0] + tvec[1]*pvec[1];
			if(F_ZERO(out[1], precision))
				out[1] = 0.f;

			if(F_EQUAL(out[1], det, precision))
				out[1] = det;

			if(det > 0.f) {
				if(F_LESS_ZERO(out[1]) || F_MORE_ZERO(out[1]-det))
					return false;
			} else {
				if(F_MORE_ZERO(out[1]) || F_LESS_ZERO(out[1]-det))
					return false;
			}

			out[2] = qvec;
			if(F_ZERO(out[2], precision))
				out[2] = 0.f;

			if(F_EQUAL(out[2], det, precision))
				out[2] = det;

			if(F_EQUAL(out[1]+out[2], det, precision))
				out[2] = det - out[1];

			if(det > 0.f) {
				if(F_LESS_ZERO(out[2]) || F_MORE_ZERO(out[1]+out[2]-det))
					return false;
			} else {
				if(F_MORE_ZERO(out[2]) || F_LESS_ZERO(out[1]+out[2]-det))
					return false;
			}

			out[1] *= inv_det;
			out[2] *= inv_det;
			out[0] = 1.f - out[1] - out[2];
			return true;
		}

		inline bool ptInTriangle(f32 pt_x, f32 pt_y, f32 ax, f32 ay, f32 bx, f32 by, f32 cx, f32 cy) {
			f32 x_ab = bx - ax;
			f32 y_ab = by - ay;

			f32 x_bc = cx - bx;
			f32 y_bc = cy - by;

			f32 x_ac = ax - cx;
			f32 y_ac = ay - cy;

			if(x_ab*y_bc - y_ab*x_bc > 0) {
				f32 dx = pt_x - bx;
				f32 dy = pt_y - by;

				if(x_ab*dy - y_ab*dx < 0)
					return false;

				dx = pt_x - cx;
				dy = pt_y - cy;
				if(x_bc*dy - y_bc*dx < 0)
					return false;

				dx = pt_x - ax;
				dy = pt_y - ay;
				if(x_ac*dy - y_ac*dx < 0)
					return false;

				return true;
			} else if(x_ab*y_bc - y_ab*x_bc < 0) {
				f32 dx = pt_x - bx;
				f32 dy = pt_y - by;

				if(x_ab*dy - y_ab*dx > 0)
					return false;

				dx = pt_x - cx;
				dy = pt_y - cy;
				if(x_bc*dy - y_bc*dx > 0)
					return false;

				dx = pt_x - ax;
				dy = pt_y - ay;
				if(x_ac*dy - y_ac*dx > 0)
					return false;

				return true;
			} else {
				//	ASSERT(false);
				return false;
			}
		}
		inline static bool rayThroughTriangle(const f32* rayOrg, 
			const f32* rayDstBaseOrg,
			const f32* a, 
			const f32* b, 
			const f32* c,
			f32* ar,
			f32* br,
			f32* cr,
			f32* t,
			bool normalize = true) {
			f32 dir[3] = {rayDstBaseOrg[0], rayDstBaseOrg[1], rayDstBaseOrg[2]};
			if(normalize)
				normalizeVector(dir);

			// find vectors for two edges sharing vert0 
			f32 edge1[3] = {b[0] - a[0], b[1] - a[1], b[2] - a[2]};
			f32 edge2[3] = {c[0] - a[0], c[1] - a[1], c[2] - a[2]};
			// begin calculating determinant - also used to calculate U parameter 
			f32 pvec[3]; crossVector(dir, edge2, pvec);
			// if determinant is near zero, ray lies in plane of triangle 
			f32 det = dotVector(edge1, pvec);
			f32 inv_det = 1.f/det;
			// calculate distance from vert0 to ray origin 
			f32 tvec[3] = {rayOrg[0] - a[0], rayOrg[1] - a[1], rayOrg[2] - a[2]};
			f32 qvec[3]; crossVector(tvec, edge1, qvec);

			if(F_ZERO(det))
				return false;

			*br = dotVector(tvec, pvec);
			if(F_ZERO(*br)) *br = 0.f;
			if(F_EQUAL(*br, det)) *br = det;

			if(det > 0.f) {
				if(*br < 0.f || *br > det)
					return false;
			} else {
				if(*br > 0.f || *br < det)
					return false;
			}

			// calculate V parameter and test bounds 
			*cr = dotVector(dir, qvec);
			if(F_ZERO(*cr)) *cr = 0.f;
			if(F_EQUAL(*cr, det)) *cr = det;

			if(det > 0.f) {
				if(*cr < 0.f || (*br+*cr) > det)
					return false;
			} else {
				if(*cr > 0.f || (*br+*cr) < det)
					return false;
			}


			f32 tf = dotVector(edge2, qvec) * inv_det;

			f32 tt = F_ZERO(rayDstBaseOrg[0]) ? 
        (F_ZERO(rayDstBaseOrg[1]) ? tf*dir[2]/rayDstBaseOrg[2] : tf*dir[1]/rayDstBaseOrg[1]) 
				: tf*dir[0]/rayDstBaseOrg[0];

			if(t) *t = tt;
			(*br) *= inv_det;
			(*cr) *= inv_det;
			(*ar) = 1.f - *br - *cr;
			return tt > 0.f && tt <= 1.f;
		}
		template<class T>
		inline f32 getFactor(const T& min, const T& max, const T& val) {
			return (val-min)/(f32)(max-min);
		}
		template <typename T>
		inline T* stepPointer(T* ptr, int stride) {
			return reinterpret_cast<T*>(reinterpret_cast<u8*>(ptr) + stride);
		}
		template <typename T>
		inline const T* stepPointer(const T* ptr, int stride) {
			return reinterpret_cast<const T*>(reinterpret_cast<const u8*>(ptr) + stride);
		}
		template<typename T, typename U>
		class _DefaultCompare_ {
		public:
			inline static int compare(const T& v1, const U& v2) { return v1 == v2 ? 0 : (v1 > v2 ? 1 : -1); }
		};
		template<typename T, typename U, typename Cmp>
		inline int binary_search(T* src, u32 n, U val, Cmp cmp, u32* hint = NULL) {
			int s = 0;
			int e = n;
			while (e > s) {
				int mid = (s+e)>>1;
				int c = cmp.compare(src[mid], val);
				if(c == 0) {
					if(hint) *hint = mid;
					return mid;
				} else if(c > 0)
					e = mid;
				else 
					s = mid+1;
			}

			if(hint) *hint = e;//CLAMP((u32)e, 0, n-1);
			return -1;
		}
		template<typename T, typename U>
		inline int binary_search(T* src, u32 n, U val, u32* hint = NULL) {
			return binary_search(src, n , val, _DefaultCompare_<T, U>(), hint);
		};
		template<typename T, typename Cmp>
		inline void bubble_sort(T* p, u32 n, Cmp cmp) {
			if(n == 0 || n == 1)
				return;

			for (int i = n-1; i > 0; --i) {
				for (int k = 0; k < i; ++k) {
					if(cmp.compare(p[k], p[k+1]) > 0) {
						T tmp = p[k];
						p[k] = p[k+1];
						p[k+1] = tmp;
					}
				}
			}
		}
		template<typename T> 
		inline void bubble_sort(T* p, u32 n) {
			bubble_sort(p, n, _DefaultCompare_<T, T>());
		}
		template<typename T, typename U>
		class _DefaultLess_ {
		public:
			static bool isLess(const T& v1, const U& v2) { return v1 < v2; }
		};
		template<typename T, typename _Less>
		inline void heap_sink(T* ary, u32 element, u32 max, _Less cmp) {
			while ((element<<1) < max) {
				u32 j = (element<<1);
				if(j+1 < max && cmp.isLess(ary[j], ary[j+1]))
					j = j+1;

				if(_Less::isLess(ary[element], ary[j])) {
					T t = ary[j];
					ary[j] = ary[element];
					ary[element] = t;
					element = j;
				}
				else
					return;
			}
		}

		template<typename T, typename _Less>
		inline void heap_sort(T* p, u32 n, _Less cmp) {
			T*  vp = p - 1;
			u32 vn = n + 2;
			int i;

			for (i = ((n-1)/2); i >= 0; --i)
				heap_sink(vp, i+1, vn-1, cmp);

			for (i = n-1; i >= 0; --i) {
				T t = p[0];
				p[0] = p[i];
				p[i] = t;
				heap_sink(vp, 1, i + 1, cmp);
			}
		}
		template<typename T>
		inline void heap_sort(T* p, u32 n) { heap_sort(p, n,  _DefaultLess_<T, T>()); }
  } //end namespace util
} //end namespace six
#endif//__SIX_UTILITY_H_INCLUDED_