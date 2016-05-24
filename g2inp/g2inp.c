#define EXTERN							      /*  for headers */
#define OLD_USAGE		                                      /*  for old/new conflicts */
#define CVS_VERSION

/*
 *      g2inp main routine
C*      Copyright University Corporation for Atmospheric Research, 1995   *
 */

#if(0)
#ifdef CVS_VERSION
#include <Xan_general.h>
#include <Xan_file.h>
#endif
#endif

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/openmenu.h>
#include <xview/notice.h>
#include <stdio.h>
#include <string.h>
#include "lib/defines.h"
#include "Xlib/save_restore.h"
#include "Xlib/gauge.h"

#include "Xlib/Xanadu_Xview_declarations"
Frame	frame;
Panel 	panel;
Panel_item  item_start, item_end, item_dapfile, item_genprofile, 
	    item_segment, item_user, item_note;

int	iquit=0;
int	ipc = 0;
int	batch_mode = FALSE;
int	start_time, end_time;
char	dapfile[80],genprofile[MAX_FILE_NAME_LENGTH],notes[300],username[50];
char	*drgv = "g2inp";
int	datainterval, overwriteflag;
int     freq_index = 1;
int	genpro_segment_to_use = 1;
int	saveflag = 0;
int	formatted = 0;
int	flag;

#include "Xlib/Xanadu_function_declarations"
int	fquit();
void	master(); 
void    pad();
void	process(); 
void		save_def();
int             restore();
int		newfile_();
int		perrr_();
void            show_dir();
void            setfile();
void            write_defaults();

char            title_line[MAXLINE];
char            file_specs[MAXLINE];	/* plot specs            */
FILE	       *specs_file;
			/* include initial values for specs here */
int             zero = 0, one = 1, end_def = 300000, points_def = 600;
float		fone = 1.;
			/* plot specifications in spec file      */
struct specifications {
    char           *keyword;
    char            type;
    int            *variable_address;
    char           *window_item_name;
    int            *default_value;
}               spec[] = {
    "START", INT, &start_time, "START:", &zero,
    "END", INT, &end_time, "END:", &end_def,
    "GENF", STR, (int *) genprofile, "GENPRO FILE (<120 char):", (int *) genprofile,
    "USER", STR, (int *) username, "USER NAME:", (int *) " ",
    "SAVEHDR", INT, &saveflag, "Save GENPRO header in file 'fort.39'?",
		&saveflag,
    "FORMATTED", INT, &formatted, "Formatted file (not binary)?", &formatted,
    "GSEGMENT", INT, &genpro_segment_to_use, "GENPRO segment to use:", &genpro_segment_to_use,
    "COMMENT", LIN, (int *) notes, "NOTE (<80 char):", (int *) " ",
    "FREQ", INT, &freq_index, "DATA FREQUENCY:", &one,
};

int             number_specifications = sizeof(spec) / sizeof(struct specifications);


#include "Xlib/set_panel.c"
#include "Xlib/save_restore_3.c"

main(argc, argv)
    int		argc;
    int	       *argv[];

{
    void	quit(); 
    int		toggle_selected();
    int		selected();
    int		numeric_text();
    int		text();
    int 	brgc;
    char 	*brgv[30];
    int		i,j;

    if(argc > 1) batch_mode = TRUE;
    strncpy(dapfile, (char *) getenv("XANFILE"), 5);
    for(i=0;i<5;i++) {
        if(!isalnum(dapfile[i])) {
	    for(j=i;j<5;j++) 
                dapfile[j] = BLANK;
	    break;
	}
    }
    sprintf(genprofile,"%s/G%s",(char *) getenv("MIGS"), 
	(char *) getenv("XANFILE"));
    for(i=1; i<MAX_FILE_NAME_LENGTH; i++) {
        if(!isalnum(genprofile[i]) && 
				 (genprofile[i] != '/')) {
	    for(j=i; j<MAX_FILE_NAME_LENGTH; j++)
                genprofile[j] = BLANK;
	    break;
	}
    }
			/*
			 * set the default values from g2inp.def
			 */
    
    strcpy(file_specs, "g2inp.def");
    (void) restore(file_specs);
    printf("genprofile length=%d, name=%s\n", strlen(genprofile), genprofile);
    genprofile[strlen(genprofile)] = ' ';

    if(batch_mode) {
        process();
        exit(0);
    }
    brgv[0] = "g2inp";
    brgc = 3;
    brgv[1] = "-geometry";
    brgv[2] = "+0-0";
    brgv[3] = (char *) NULL;
    (void)xv_init(XV_INIT_ARGC_PTR_ARGV, &brgc, brgv, NULL);

    iquit = 0;
    frame = xv_create(XV_NULL, FRAME,
	FRAME_LABEL,		brgv[0],
	XV_WIDTH,		1150,
	XV_HEIGHT,		140,
	FRAME_SHOW_FOOTER,	TRUE,
	NULL);
    panel = (Panel)xv_create(frame, PANEL, NULL);
    xv_set(canvas_paint_window(panel), NULL);
    xv_create(panel, PANEL_BUTTON,
	PANEL_LABEL_STRING,     "PROCESS",
	PANEL_NOTIFY_PROC,      process,
	PANEL_CLIENT_DATA,	frame,
	XV_HELP_DATA,		"g2inp:PROCESS",
	NULL);
#include "Xlib/quit.button"
    item_dapfile = (Panel_item) xv_create(panel, PANEL_TEXT,
	PANEL_LABEL_STRING,	"DAP FILE (5 char):",
	PANEL_VALUE,		dapfile,
	PANEL_NOTIFY_PROC,	text,
	PANEL_CLIENT_DATA,	frame,
	PANEL_VALUE_DISPLAY_LENGTH , 7,
	XV_HELP_DATA,		"g2inp:DAP FILE",
	NULL);
    xv_create(panel, PANEL_TOGGLE,
        PANEL_FEEDBACK,         PANEL_MARKED,
        PANEL_LABEL_STRING,     "OVERWRITE IF ALREADY THERE?",
        PANEL_VALUE,            overwriteflag, /* binary choices */
        PANEL_CHOICE_STRINGS,    "yes",NULL,
        PANEL_NOTIFY_PROC,      toggle_selected,
        PANEL_CLIENT_DATA,      frame,
	XV_HELP_DATA,		"g2inp:OVERWRITE",
        NULL);
    item_genprofile = (Panel_item) xv_create(panel, PANEL_TEXT,
	PANEL_LABEL_STRING,	"GENPRO FILE (<120 char):",
	PANEL_VALUE,		genprofile,
	PANEL_NOTIFY_PROC,	text,
	PANEL_CLIENT_DATA,	frame,
	PANEL_VALUE_DISPLAY_LENGTH , 40,
	XV_HELP_DATA,		"g2inp:GENPRO FILE",
	NULL);
#include "Xlib/startend.button"
    xv_create(panel, PANEL_CHOICE,
        PANEL_LABEL_STRING,     "DATA FREQUENCY:",
        PANEL_CHOICE_STRINGS,   "0.1","1", "2", "5", "10",
				"20","50","125","250", NULL,
	PANEL_VALUE,		freq_index,
        PANEL_NOTIFY_PROC,      selected,
        PANEL_CLIENT_DATA,      frame,
	XV_HELP_DATA,		"g2inp:FREQUENCY",
        NULL);
	item_segment = (Panel_item) xv_create(panel, PANEL_NUMERIC_TEXT,
		  PANEL_LABEL_STRING, "GENPRO segment to use:",
		  PANEL_VALUE, genpro_segment_to_use,
		  PANEL_MAX_VALUE, 1000,
		  PANEL_MIN_VALUE, 0,
		  PANEL_NOTIFY_PROC, numeric_text,
		  PANEL_VALUE_DISPLAY_LENGTH, 8,
		  PANEL_CLIENT_DATA, frame,
		  XV_HELP_DATA, "g2inp:GSEGMENT",
		  NULL);
    item_user = (Panel_item) xv_create(panel, PANEL_TEXT,
	PANEL_LABEL_STRING,	"USER NAME:",
	PANEL_VALUE,		username,
	PANEL_NOTIFY_PROC,	text,
	PANEL_CLIENT_DATA,	frame,
	PANEL_VALUE_DISPLAY_LENGTH , 16,
	XV_HELP_DATA,		"g2inp:USERNAME",
	NULL);
    xv_create(panel, PANEL_TOGGLE,
        PANEL_FEEDBACK,         PANEL_MARKED,
        PANEL_LABEL_STRING,     "Save GENPRO header in 'fort.39'?",
        PANEL_VALUE,            saveflag, /* binary choices */
        PANEL_CHOICE_STRINGS,    "yes",NULL,
        PANEL_NOTIFY_PROC,      toggle_selected,
        PANEL_CLIENT_DATA,      frame,
	XV_HELP_DATA,		"g2inp:HEADSAVE",
        NULL);
    xv_create(panel, PANEL_TOGGLE,
        PANEL_FEEDBACK,         PANEL_MARKED,
        PANEL_LABEL_STRING,     "Formatted file (not binary)?",
        PANEL_VALUE,            formatted, /* binary choices */
        PANEL_CHOICE_STRINGS,    "yes",NULL,
        PANEL_NOTIFY_PROC,      toggle_selected,
        PANEL_CLIENT_DATA,      frame,
	XV_HELP_DATA,		"g2inp:FORMATTED",
        NULL);
    item_note = (Panel_item) xv_create(panel, PANEL_TEXT,
	PANEL_LABEL_STRING,	"NOTE (<80 char):",
	PANEL_VALUE,		notes,
	PANEL_NOTIFY_PROC,	text,
	PANEL_CLIENT_DATA,	frame,
	PANEL_VALUE_DISPLAY_LENGTH , 80,
	XV_HELP_DATA,		"g2inp:NOTES",
	NULL);


    iquit = 0;

    iquit = 0;
#include "Xlib/save_restore_2.c"
#include "Xlib/instr.button"
#include "Xlib/gauge.c"
    xv_set (frameg, FRAME_LABEL, "GENPRO conversion", NULL);


    xv_main_loop(frame);
    exit(0);

}



numeric_text(item, event)
Panel_item item;
Event *event;
{
    char buf[32];
    Frame frame = xv_get(item, PANEL_CLIENT_DATA);

    sprintf(buf, "\"%s\" set to %d",
       (char *)xv_get(item, PANEL_LABEL_STRING), (int)xv_get(item, PANEL_VALUE));
    xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
    sprintf(buf, "%s",
       (char *)xv_get(item, PANEL_LABEL_STRING));
    if(!strcmp(buf,"START:"))
	{start_time = (int)xv_get(item,PANEL_VALUE);
	}
    else if(!strcmp(buf,"END:"))
	{end_time =  (int)xv_get(item,PANEL_VALUE);
	}
    else if (!strncmp(buf,"GENPRO segment",14)) {
	genpro_segment_to_use = (int) xv_get(item, PANEL_VALUE);
    }
    return PANEL_NEXT;
}

text(item, event)
Panel_item item;
Event *event;
{
    char buf[100];
    int i,j;
    Frame frame = xv_get(item, PANEL_CLIENT_DATA);


    sprintf(buf, "\"%s\" set to %s",
       (char *)xv_get(item, PANEL_LABEL_STRING), (char *)xv_get(item, PANEL_VALUE));
    xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
    if(!strcmp("DAP FILE (5 char):",(char *)xv_get(item, PANEL_LABEL_STRING)))
    	{
	strncpy(dapfile,(char *)xv_get(item,PANEL_VALUE),5);
        		{for(i=0;i<5;i++)
                		{if(!isalnum(dapfile[i])) {
				    for(j=i;j<5;j++)
                        		dapfile[j] = BLANK;
				    break;
				    }
                		}
			}
	}
    if(!strcmp("GENPRO FILE (<120 char):",(char *)xv_get(item, PANEL_LABEL_STRING)))
    	{
	strncpy(genprofile,(char *)xv_get(item,PANEL_VALUE),MAX_FILE_NAME_LENGTH);
        		{for(i=1;i<MAX_FILE_NAME_LENGTH;i++)
                		{if(!isalnum(genprofile[i]) && 
				 (genprofile[i] != '/'))
				    {for(j=i;j<MAX_FILE_NAME_LENGTH;j++)
                        		genprofile[j] = BLANK;
				    break;
				    }
                		}
			}
	}
    if(!strcmp("USER NAME:",(char *)xv_get(item, PANEL_LABEL_STRING)))
    	{
	strncpy(username,(char *)xv_get(item,PANEL_VALUE),16);
        		{for(i=0;i<16;i++)
                		{if(!isalnum(username[i])) {
				    for(j=i;j<16;j++)
                        		username[j] = BLANK;
				    break;
				    }
                		}
			}
	}
    if(!strcmp("NOTE (<80 char):",(char *)xv_get(item, PANEL_LABEL_STRING)))
    	{
	strncpy(notes,(char *)xv_get(item,PANEL_VALUE),80);
        		{for(i=0;i<80;i++)
                		{if(!isalnum(notes[i]) && notes[i]!=BLANK) {
				    for(j=i;j<80;j++)
                        		notes[j] = BLANK;
				    break;
				    }
                		}
			}
	}
    return PANEL_NEXT;
}

#include "Xlib/quit.c"

int
selected(item, value, event)
Panel_item item;
int value;
Event *event;
{
    char buf[32];
    Frame frame = xv_get(item, PANEL_CLIENT_DATA);
    if (event_id(event) == MS_LEFT) {
	freq_index = xv_get(item,PANEL_VALUE);
        sprintf(buf, "PANEL %s",
            xv_get(item, PANEL_CHOICE_STRING, panel_get_value(item)));
        xv_set(frame, FRAME_RIGHT_FOOTER, buf, NULL);
        return XV_OK;
    }
    return XV_ERROR;
}

int
toggle_selected(item, value, event)
Panel_item item;
unsigned value;
Event *event;
{
    if (event_id(event) == MS_LEFT) {
	if(!strncmp("OVERWRITE", (char *)xv_get(item, PANEL_LABEL_STRING), 9))
    	    overwriteflag = value;
	else if (!strncmp("Save GENP", 
			(char *)xv_get(item, PANEL_LABEL_STRING), 9))
	    saveflag = value;
	else if (!strncmp("Formatted", 
			(char *)xv_get(item, PANEL_LABEL_STRING), 9))
	    formatted = value;
        return XV_OK;
    }
    return XV_ERROR;
}

#include "Xlib/master.c"

int
read_all()

{
    int i, j;

    start_time = (int) xv_get(item_start, PANEL_VALUE);
    end_time =  (int) xv_get(item_end, PANEL_VALUE);
    genpro_segment_to_use = (int) xv_get(item_segment, PANEL_VALUE);
    strncpy(dapfile, (char *) xv_get(item_dapfile, PANEL_VALUE),5);
        		for(i=0;i<5;i++) {
                		if(!isalnum(dapfile[i])) {
				    for(j=i;j<5;j++)
                        		dapfile[j] = BLANK;
				    break;
                		}
			}
    strncpy(genprofile, (char *) xv_get(item_genprofile, PANEL_VALUE), MAX_FILE_NAME_LENGTH);
        		for(i=1;i<MAX_FILE_NAME_LENGTH;i++) {
                		if(!isalnum(genprofile[i]) && 
				 (genprofile[i] != '/')) {
				    for(j=i;j<MAX_FILE_NAME_LENGTH;j++)
                        		genprofile[j] = BLANK;
				    break;
				}
			}
    strncpy(username, (char *) xv_get(item_user, PANEL_VALUE), 16);
        		for(i=0;i<16;i++) {
                		if(!isalnum(username[i])) {
				    for(j=i;j<16;j++)
                        		username[j] = BLANK;
				    break;
				}
                	}
    strncpy (notes, (char *) xv_get(item_note, PANEL_VALUE), 80);
        		for(i=0;i<80;i++) {
                		if(!isalnum(notes[i]) && notes[i]!=BLANK) {
				    for(j=i;j<80;j++)
                        		notes[j] = BLANK;
				    break;
				}
                	}
	return(0);
}

void
process()

{
  int go_on = -1;
  int i,j;

  if(!batch_mode) {
    (void) read_all();
    xv_set(frame, FRAME_BUSY, TRUE, NULL);
    xv_set(frameg, XV_SHOW, TRUE, NULL);
  }
  
  if(freq_index == 0) datainterval = 10000;
  else if(freq_index == 1) datainterval = 1000;
  else if(freq_index == 2) datainterval = 500;
  else if(freq_index == 3) datainterval = 200;
  else if(freq_index == 4) datainterval = 100;
  else if(freq_index == 5) datainterval = 50;
  else if(freq_index == 6) datainterval = 20;
  else if(freq_index == 7) datainterval = 8;
  else if(freq_index == 8) datainterval = 4;
  flag = formatted * 100 + overwriteflag * 10 + saveflag;
  g2prmpt_(&start_time, &end_time, dapfile, genprofile, username, notes, 
	   &datainterval, &flag, &genpro_segment_to_use,
	   &batch_mode, &go_on, strlen(dapfile), strlen(genprofile),
	   strlen(username), strlen(notes));
  if(go_on < 0) {
    if(!batch_mode) {
      xv_set(frame, FRAME_BUSY, FALSE, NULL);
      xv_set(frameg, XV_SHOW, FALSE, NULL);
    }
    return;
  }
  g2rdata_(&batch_mode);
  if(!batch_mode) {
    xv_set(frame, FRAME_BUSY, FALSE, NULL);
    xv_destroy_safe(frame);
    exit(0);
  }
}
    
int
newfile_()
{
  int result = notice_prompt(panel, NULL,
			     NOTICE_MESSAGE_STRINGS,
			     "OUTPUT DAP FILE already exists",
			     "Select action:",
			     NULL,
			     NOTICE_BUTTON_YES, "OVERWRITE file",
			     NOTICE_BUTTON_NO, "Return to menu without processing",
			     NULL);
  
  if (result == NOTICE_YES) 
    return (-1);
  else
    return (0);
}
  
int
summary_(data_file, nsegments, bseg, eseg)
  char *data_file;
  int  *nsegments;
  int  bseg[3], eseg[3];
{
  int result;
  char string1[60], string2[60];

  sprintf(string1, "G2INP CONVERSION FINISHED, DATA FILE=%s", data_file);
  sprintf(string2, "OUTPUT TIMES are %d%02d%02d-%d%02d%02d", bseg[0],bseg[1],bseg[2],eseg[0],eseg[1],eseg[2]);
  if (!batch_mode) {
      result = notice_prompt(panel, NULL,
			     NOTICE_MESSAGE_STRINGS,
			     string1,
			     string2,
			     NULL,
			     NOTICE_BUTTON_YES, "DISMISS",
			     NULL);
  }
  
  return (-1);
}
  
int
  perrr_()
{
    int result = notice_prompt(panel, NULL,
       NOTICE_MESSAGE_STRINGS,
       "Too many errors reading header file.",
       "Program is aborting.  Check that file",
       "is correct format (usually, binary,",
       "but formatted for some N2UW files).",
       NULL,
       NOTICE_BUTTON_YES, "DISMISS this window",
       NULL);
 
    if (result == NOTICE_YES) 
        return (-1);
    else
	return (0);
}
  
#if(0)				/* use version from arc.c instead */
#include "Xlib/pad.c"
#endif
#include "Xlib/setgg.c"
#include "Xlib/instr_callback.c"
