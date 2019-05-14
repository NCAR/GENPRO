
/*
**      $Id: conversion.h,v 1.1 2000/02/18 18:04:17 morreale Exp $
*/
/************************************************************************
*									*
*			     Copyright (C)  1999			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
**	File:		conversion.h
**
**	Author:		Peter Morreale
**			National Center for Atmospheric Research
**			PO 3000, Boulder, Colorado
**
**	Date:		Wed Jul 21 10:31:25 MDT 1999
**
**	Description:	Headers for conversion routines..
*/

#ifndef _conversion_
#define _conversion_


extern void ctodpf(
	void	*in_array,
	void	*out_array,
	int 	*n
);
extern void ctospf(
	void	*in_array,
	void	*out_array,
	int 	*n
);
extern void ctospi(
	void	*in_array,
	void	*out_array,
	int 	*n,
	int 	*zpad
);

extern void dptocf(
	void	*in_array,
	void	*out_array,
	int 	*n
);

extern void dptoci(
	void	*in_array,
	void	*out_array,
	int	 *n
);

extern void sptocf(
	void	*in_array,
	void	*out_array,
	int 		*n
);

extern void sptoci(
	void	*in_array,
	void	*out_array,
	int 	*n,
	int	*zpad
);

/* Do not add anything below this line */
#endif
