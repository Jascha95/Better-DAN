#ifndef CONCAT_H_
#define CONCAT_H_

/* length of a null-terminated string */
size_t mstrlen(const char *s);

/* concatenate two null-terminated strings into a fresh string */
char * mconcat (const char *s1, const char *s2);

#endif // CONCAT_H_
