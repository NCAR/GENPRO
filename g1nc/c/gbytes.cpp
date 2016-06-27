// From http://rda.ucar.edu/libraries/gbytes/gbytes.cpp

#include <stdlib.h>
#include <iostream> // NSD 5/26/16

using namespace std; // NSD 5/26/16

template <class MaskType>
inline void createMask(MaskType& mask,size_t size)
{
  size_t n;

  mask=1;
  for (n=1; n < size; n++)
    (mask<<=1)++;
}

template <class BufType,class LocType>
inline void getBits(const BufType *buf,LocType *loc,size_t off,
  const size_t bits,size_t skip = 0,const size_t num = 1)
{
// create a mask to use when right-shifting (necessary because different
// compilers do different things when right-shifting a signed bit-field)
  BufType bmask;
  size_t loc_size=sizeof(LocType)*8,buf_size=sizeof(BufType)*8,wskip;
  int rshift;
  LocType lmask,temp;

  if (bits > loc_size) {
    cerr << "Error: unpacking " << bits << " bits into a " << loc_size <<
      "-bit field" << endl;
    exit(1);
  }
  else {
    createMask(bmask,buf_size);
    skip+=bits;
    if (loc_size <= buf_size) {
	for (size_t n=0; n < num; n++) {
// skip to the word containing the packed field
	  wskip=off/buf_size;
// right shift the bits in the packed buffer word to eliminate unneeded bits
	  rshift=buf_size-(off % buf_size)-bits;
// check for a packed field spanning two words
	  if (rshift < 0) {
	    loc[n]=(buf[wskip]<<-rshift);
	    loc[n]+=(buf[++wskip]>>(buf_size+rshift))&~(bmask<<-rshift);
	  }
	  else
	    loc[n]=(buf[wskip]>>rshift);
// remove any unneeded leading bits
	  if (bits != buf_size) loc[n]&=~(bmask<<bits);
	  off+=skip;
	}
    }
    else {
	createMask(lmask,loc_size);
// get difference in bits between word size of packed buffer and word size of
// unpacked location
	for (size_t n=0; n < num; n++) {
// skip to the word containing the packed field
	  wskip=off/buf_size;
// right shift the bits in the packed buffer word to eliminate unneeded bits
	  rshift=buf_size-(off % buf_size)-bits;
// check for a packed field spanning multiple words
	  if (rshift < 0) {
	    loc[n]=0;
	    while (rshift < 0) {
		temp=buf[wskip++];
		loc[n]+=(temp<<-rshift);
		rshift+=buf_size;
	    }
	    if (rshift != 0)
		loc[n]+=(buf[wskip]>>rshift)&~(bmask<<(buf_size-rshift));
	    else
		loc[n]+=buf[wskip];
	  }
	  else
	    loc[n]=(buf[wskip]>>rshift);
// remove any unneeded leading bits
	  if (bits != loc_size) loc[n]&=~(lmask<<bits);
	  off+=skip;
	}
    }
  }
}

template <class BufType,class LocType>
void gbytes(const BufType *buf,LocType *loc,const size_t off,const size_t bits,const size_t skip,const size_t num)
{
// no work to do
  if (bits == 0) return;

  getBits(buf,loc,off,bits,skip,num);
}

template <class BufType,class LocType>
void gbyte(const BufType *buf,LocType& loc,const size_t off,const size_t bits)
{
// no work to do
  if (bits == 0) return;

  getBits(buf,&loc,off,bits);
}

template <class BufType,class SrcType>
inline void putBits(BufType *buf,const SrcType *src,size_t off,const size_t bits,size_t skip = 0,const size_t num = 1)
{
// create a mask to use when right-shifting (necessary because different
// compilers do different things when right-shifting a signed bit-field)
  size_t src_size=sizeof(SrcType)*8,buf_size=sizeof(BufType)*8;
  size_t n,wskip,bskip,lclear,rclear,more;
  BufType bmask,left,right;

  if (bits > src_size)
    cerr << "Error: packing " << bits << " bits from a " << src_size << "-bit field" << endl;
  else {
    createMask(bmask,buf_size);
    skip+=bits;
    for (n=0; n < num; n++) {
// get number of words and bits to skip before packing begins
	wskip=off/buf_size;
	bskip=off % buf_size;
	lclear=bskip+bits;
	rclear=buf_size-bskip;
	left= (rclear != buf_size) ? (buf[wskip]&(bmask<<rclear)) : 0;
	if (lclear <= buf_size) {
// all bits to be packed are in the current word
// clear the field to be packed
	  right=
          (lclear != buf_size) ? (buf[wskip]&~(bmask<<(buf_size-lclear))) : 0;
// fill the field to be packed
	  buf[wskip]= (buf_size > src_size) ? src[n]&~(bmask<<src_size) : src[n];
	  buf[wskip]=left|right|(buf[wskip]<<(rclear-bits));
	}
	else {
// bits to be packed cross a word boundary(ies)
// clear the bit field to be packed
	  more=bits-rclear;
	  if (rclear != buf_size)
	    buf[wskip]&=~(bmask<<rclear);
	  buf[wskip]=left|(src[n]>>more);
// clear the next (or part of the next) word and pack those bits
	  while (more > buf_size) {
	    more-=buf_size;
	    buf[++wskip]=src[n]>>more;
	  }
	  wskip++;
	  more=buf_size-more;
	  right= (more != buf_size) ? (buf[wskip]&~(bmask<<more)) : 0;
	  buf[wskip]= (buf_size > src_size) ? src[n]&~(bmask<<src_size) : src[n];
	  buf[wskip]=right|(buf[wskip]<<more);
	}
	off+=skip;
    }
  }
}

template <class BufType,class SrcType>
void sbytes(BufType *buf,const SrcType *src,const size_t off,const size_t bits,const size_t skip = 0,const size_t num = 1)
{
// no work to do
  if (bits == 0) return;

  putBits(buf,src,off,bits,skip,num);
}

template <class BufType,class SrcType>
void sbyte(BufType *buf,const SrcType src,const size_t off,const size_t bits)
{
// no work to do
  if (bits == 0) return;

  putBits(buf,&src,off,bits);
}
