// By far, the trickiest FSM compression scheme, but it works best when
//   the data is sparcely spaced.  This scheme uses a list of pointer arrays.
//   Each array has its own variable name.

unsigned char Yy_nxt0 [];
unsigned char Yy_nxt1 [];
unsigned char Yy_nxt2 [];
unsigned char Yy_nxt3 [];
unsigned char Yy_nxt4 [];
unsigned char Yy_nxt5 [];

unsigned char *Yy_nxt[6] =
{
	Yy_nxt0, Yy_nxt1, Yy_nxt2, Yy_nxt3, Yy_nxt4, Yy_nxt5
};

unsigned char yy_next(int cur_state, char c)
{

}
