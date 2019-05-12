#ifndef VN_ASSERT_H
#define VN_ASSERT_H

#ifndef VN_LIB_H
#error vn_assert.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifndef VN_ASSERT
#ifdef __ICC
#ifdef NDEBUG
#define VN_ASSERT(cond) __assume(cond)
#else /* !NDEBUG */
#define VN_ASSERT(cond) assert(cond)
#endif /* NDEBUG */
#else /* !__ICC */
#define VN_ASSERT(cond) assert(cond)
#endif /* __ICC */
#else /* VN_ASSERT */
#error VN_ASSERT already defined
#endif /* !VN_ASSERT */

#endif /* !VN_ASSERT_H */
