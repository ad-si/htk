/* Writes and gets responses from wish.  wish is started, if it hasn't
   already. 
   */

/* must be called before either of the other two functions. */
void initialise_wish(char *wish_path);

/* writes something to the wish process */
void write_to_wish(const char *toWrite,size_t nChars);

/* gets the FD for the wish process.  Hopefully we will then be able to wait
   on it. */
int get_readwish_fd(void);

/* reads from wish.  NB - this will block until something is available. */
size_t read_from_wish(char *buffer,size_t bufferSize);


