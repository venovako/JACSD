#ifndef VN_TIMER_H
#define VN_TIMER_H

#ifndef VN_LIB_H
#error vn_timer.h not intended for direct inclusion
#endif /* !VN_LIB_H */

VN_EXTERN_C vn_integer_8 vn_get_thread_ns();
VN_EXTERN_C vn_integer_8 vn_get_sys_us();

#endif /* !VN_TIMER_H */
