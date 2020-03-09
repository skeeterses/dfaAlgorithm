// The same FSM as in the first example.
//   Here, we use the reduced row/column compression format

unsigned char Yy_cmap[128] =
{

};

unsigned char Yy_rmap[6] =
{
	0, 1, 2,3, 4, 4
};

unsigned char Yy_nxt[5][4] =
{
/* 00 */  { },
/* 01 */  { },
/* 02 */  { },
/* 03 */  { },
/* 04 */  { }
};

unsigned char yy_next(int state, char c)
{
  return Yy_nxt[ Yy_rmap[state] ][ Yy_cmap[c] ];
}
