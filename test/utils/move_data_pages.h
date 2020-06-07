#ifndef MOVE_DATA_PAGES_H
#define MOVE_DATA_PAGES_H

#include <stdlib.h>

// C/C++ interface
extern
#ifdef __cplusplus
"C"
#endif /* __cplusplus */
long move_data_pages(void *const addr, const size_t sz, const int to_node);

// Fortran interface
extern
#ifdef __cplusplus
"C"
#endif /* __cplusplus */
void move_data_pages_(void *const addr, const size_t sz[static 1], const int to_node[static 1], long info[static 1]);

#endif /* !MOVE_DATA_PAGES_H */
