#ifndef MOVE_DATA_PAGES_H
#define MOVE_DATA_PAGES_H

/* C/C++ interface */
extern
#ifdef __cplusplus
"C"
#endif /* __cplusplus */
long move_data_pages(void *const addr, const size_t sz, const int to_node);

/* Fortran interface */
extern
#ifdef __cplusplus
"C"
#endif /* __cplusplus */
void move_data_pages_(void *const addr, const size_t *const sz, const int *const to_node, long *const info);

#endif /* !MOVE_DATA_PAGES_H */
