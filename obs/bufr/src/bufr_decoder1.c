#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
/* bufr_decoder.c is a BUFR decoder for GOES HD Winds BUFR files. On 02/24/2004, 
  Yi Song (RSIS) developed it on the basis of the two seperate programs (bufry.c:
  seperate the whole BUFR file into each messages; and jaime_wind_decoder.c: 
  a BUFR decoder for only one message, but doesn't work for only one data recoder). 
  This program can decode not only the one BUFR message, but also the whole BUFR
  file (include multiple BUFR messages). Depending on the parameter output_type, 
  the output file can be only HD Winds data or the HD Winds data with the BUFR 
  section informations. It works for each GOES HD Winds BUFR products. If there is
  anything wrong, please send email to Yi.Song@noaa.gov. */
 
/*  useage as: gcc -lm bufr_decoder.c -o bufr_decoder
               bufr_decoder BUFR_File Output_File */

/* output_type decides the decoder output contents. For OUTPUT_TYPE=0,
   the decoder will decode the BUFR file and produce the explanation for 
   each message in its order. For OUTPUT_TYPE=1, the decoder will produce
   the BUFR data only. */ 

int OUTPUT_TYPE = 1; /* Decides the output format and contents. */ 

void message_decoder(char *, long int, int, int, FILE *);

int main(int argc, char *argv[])
{
  FILE *fpInputFile;
  FILE *fpOutputFile;
  char *OutputFile;
  struct stat stFileStat;
  char *pzWholeFile;
  char *pzOneBulletin;
  long int liFileSize;
  long int liStartOff;
  long int liOffSet;
  long int liBulLen;
  long int liLoop;
  int iState;
  long int liNumberS, liNumberT, liNumberU;
  long int liSubSets;
  int iBulletin;
  short shThou, shHund, shTens, shUnit;
  char azFile[11];

  if (argc < 2)
  {
    (void)printf("Wrong number of arguments: %d, there should be 1.\n", --argc);
    return 1;
  }

  (void)stat(*(argv + 1), &stFileStat);
  liFileSize = (long int)stFileStat.st_size;

  pzWholeFile = (char *)malloc((size_t)liFileSize);

  if ((fpInputFile = fopen(*(argv + 1), "r")) == NULL)
  {
    (void)printf("Cant open file, %s\n", *(argv + 1));
    return 2;
  }
  if (argc == 3) 
  {
  if ((fpOutputFile = fopen(*(argv + 2), "w")) == NULL)
  {
    (void)printf("Cant open file, %s\n", *(argv + 2));
    return 2;
  }
  }
  else { 
  OutputFile=(char *)malloc(strlen(*(argv + 1)) + 8);
  strcpy(OutputFile, *(argv + 1));
  strcat(OutputFile, "_decode");
    (void)printf("Output file, %s\n", OutputFile);
  if ((fpOutputFile = fopen(OutputFile, "w")) == NULL)
  {
    (void)printf("Cant open file, %s\n", OutputFile);
    return 2;
  }
      }
  iState = fread((void *)pzWholeFile, (size_t)liFileSize, 1, fpInputFile);
  if (iState == 0) printf("Number one problem\n");

  iState = fclose(fpInputFile);
  if (iState != 0) printf("Number two problem\n");

  liStartOff = 0;
  iBulletin = 0;
  do
  {
    liOffSet = -1;
    for (liLoop = liStartOff; liLoop < (liFileSize - 4); liLoop++)
    {
      if ((*(pzWholeFile + liLoop) == 'B') &&
      (*(pzWholeFile + liLoop + 1) == 'U') &&
      (*(pzWholeFile + liLoop + 2) == 'F') &&
      (*(pzWholeFile + liLoop + 3) == 'R'))
      {
        liOffSet = liLoop;
        break;
      }
    }
    if ((liOffSet < 0) || (liOffSet > liFileSize)) break;

    liNumberS = *(pzWholeFile + liOffSet + 4);
    if (liNumberS < 0) liNumberS += 256;
    liNumberT = *(pzWholeFile + liOffSet + 5);
    if (liNumberT < 0) liNumberT += 256;
    liNumberU = *(pzWholeFile + liOffSet + 6);
    if (liNumberU < 0) liNumberU += 256;
    liBulLen = (liNumberS * 256 * 256) + (liNumberT * 256) + liNumberU;
    if ((liOffSet + liBulLen) > liFileSize)
    {
       liStartOff = liOffSet + 1;
       continue;
    }
    if ((*(pzWholeFile + liOffSet + liBulLen - 4) != '7') ||
        (*(pzWholeFile + liOffSet + liBulLen - 3) != '7') ||
        (*(pzWholeFile + liOffSet + liBulLen - 2) != '7') ||
        (*(pzWholeFile + liOffSet + liBulLen - 1) != '7'))
    {
       liStartOff = liOffSet + 1;
       continue;
    }

    liNumberS = *(pzWholeFile + liOffSet + 30);
    if (liNumberS < 0) liNumberS += 256;
    liNumberT = *(pzWholeFile + liOffSet + 31);
    if (liNumberT < 0) liNumberT += 256;
    liSubSets = (liNumberS * 256) + liNumberT;

    printf("BUFR bulletin %d in %s starts at byte %ld\n", iBulletin,
      *(argv + 1), (liOffSet + 1));
    printf("Bulletin contains %ld bytes.\n", liBulLen);
    printf("Bulletin contains %ld subsets.\n", liSubSets);
    printf("BUFR ends at byte %ld\n\n", (liOffSet + liBulLen));

    pzOneBulletin = (char *)malloc((size_t)liBulLen);
    (void)memcpy(pzOneBulletin, pzWholeFile + liOffSet, (size_t)liBulLen);
     message_decoder(pzOneBulletin, (size_t)liBulLen, iBulletin, OUTPUT_TYPE, fpOutputFile);
    free((void *)pzOneBulletin);

    liStartOff = liOffSet + liBulLen;
    ++iBulletin;

  } while (1 > 0);
    iState = fclose(fpOutputFile);
    if (iState != 0) printf("Number two problem\n");

  free((void *)pzWholeFile);

}
  
void message_decoder(char *pzWholeFile, long int BulLen, int N_message, int iBulletin, FILE *Out_File) 
{
#define NDESCS 242
#define unsign(x) if (x < 0) x += 256

struct Descs
{
  short int shIndex;
  char *pzName;
  char *pzUnits;
  short int shScale;
  long int liRef;
  short int shWidth;
}
  sDescs[NDESCS] =
{
  1, "SATELLITE ID",                        "N/A",  0,   0,  10,
  2, "ORIG CENTRE ID",                      "N/A",  0,   0,  16,
  3, "SATELLITE CLASSIFICATION",            "N/A",  0,   0,   9,
  4, "SEGMENT SIZE AT NADIR IN X DIRN",       "m",  0,   0,  18,
  5, "SEGMENT SIZE AT NADIR IN Y DIRN",       "m",  0,   0,  18,
  6, "YEAR",                                "N/A",  0,   0,  12,
  7, "MONTH",                               "N/A",  0,   0,   4,
  8, "DAY",                                 "N/A",  0,   0,   6,
  9, "HOUR",                                "N/A",  0,   0,   5,
  10, "MINUTE",                             "N/A",  0,   0,   6,
  11, "SECOND",                             "N/A",  0,   0,   6,
  12, "LATITUDE (HIGH ACCURACY)",           "N/A",  5, -9000000, 25,
  13, "LONGITUDE (HIGH ACCURACY)",          "N/A",  5, -18000000,   26,
  14, "SAT INSTRUMENT DATA USED IN PROC",   "F/T",  0,   0,  31,
  15, "SAT DERIVED WIND COMP METHOD",       "C/T",  0,   0,   4,
  16, "PRESSURE",                            "Pa", -1,   0,  14,
  17, "WIND DIRECTION",                     "Deg",  0,   0,   9,
  18, "WIND SPEED",                         "m/s",  1,   0,  12,
  19, "SATELLITE CHANNEL CENTRE FREQUENCY",  "Hz", -8,   0,  26,
  20, "SATELLITE CHANNEL BAND WIDTH",        "Hz", -8,   0,  26,
  21, "COLDEST CLUSTER TEMPERATURE",          "K",  1,   0,  12,
  22, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  23, "TRACER CORRELATION METHOD",          "C/T",  0,   0,   3,
  24, "LAND/SEA QUALIFIER",                 "C/T",  0,   0,   2,
  25, "SATELLITE ZENITH ANGLE",             "Deg",  2, -9000,  15,
  26, "ORIGIN OF FIRST GUESS INFORMATION",  "C/T",  0,   0,   4,
  27, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  28, "YEAR",	                            "N/A",  0,   0,  12,
  29, "MONTH",                              "N/A",  0,   0,   4,
  30, "DAY",	                            "N/A",  0,   0,   6,
  31, "HOUR",	                            "N/A",  0,   0,   5,
  32, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  33, "TIME PERIOD OR DISPLACEMENT",       "Hour",  0, -2048,  12,
  34, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  35, "HOUR",	                            "N/A",  0,   0,   5,
  36, "MINUTE",                             "N/A",  0,   0,   6,
  37, "SECOND",                             "N/A",  0,   0,   6,
  38, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  39, "HOUR",	                            "N/A",  0,   0,   5,
  40, "MINUTE",                             "N/A",  0,   0,   6,
  41, "SECOND",                             "N/A",  0,   0,   6,
  42, "WIND DIRECTION",                     "Deg",  0,   0,   9,
  43, "WIND SPEED",                         "m/s",  1,   0,  12,
  44, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  45, "HOUR",	                            "N/A",  0,   0,   5,
  46, "MINUTE",                             "N/A",  0,   0,   6,
  47, "SECOND",                             "N/A",  0,   0,   6,
  48, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  49, "HOUR",	                            "N/A",  0,   0,   5,
  50, "MINUTE",                             "N/A",  0,   0,   6,
  51, "SECOND",                             "N/A",  0,   0,   6,
  52, "WIND DIRECTION",                     "Deg",  0,   0,   9,
  53, "WIND SPEED",                         "m/s",  1,   0,  12,
  54, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  55, "HOUR",	                            "N/A",  0,   0,   5,
  56, "MINUTE",                             "N/A",  0,   0,   6,
  57, "SECOND",                             "N/A",  0,   0,   6,
  58, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  59, "HOUR",	                            "N/A",  0,   0,   5,
  60, "MINUTE",                             "N/A",  0,   0,   6,
  61, "SECOND",                             "N/A",  0,   0,   6,
  62, "WIND DIRECTION",                     "Deg",  0,   0,   9,
  63, "WIND SPEED",                         "m/s",  1,   0,  12,
  64, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  65, "HOUR",	                            "N/A",  0,   0,   5,
  66, "MINUTE",                             "N/A",  0,   0,   6,
  67, "SECOND",                             "N/A",  0,   0,   6,
  68, "TIME SIGNIFICANCE",                  "C/T",  0,   0,   5,
  69, "HOUR",	                            "N/A",  0,   0,   5,
  70, "MINUTE",                             "N/A",  0,   0,   6,
  71, "SECOND",                             "N/A",  0,   0,   6,
  72, "WIND DIRECTION",                     "Deg",  0,   0,   9,
  73, "WIND SPEED",                         "m/s",  1,   0,  12,
  74, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  75, "PRESSURE",                            "Pa", -1,   0,  14,
  76, "TEMPERATURE/DRY BULB TEMPERATURE",     "K",  1,   0,  12,
  77, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  78, "PRESSURE",                            "Pa", -1,   0,  14,
  79, "TEMPERATURE/DRY BULB TEMPERATURE",     "K",  1,   0,  12,
  80, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  81, "PRESSURE",                            "Pa", -1,   0,  14,
  82, "TEMPERATURE/DRY BULB TEMPERATURE",     "K",  1,   0,  12,
  83, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  84, "PRESSURE",                            "Pa", -1,   0,  14,
  85, "TEMPERATURE/DRY BULB TEMPERATURE",     "K",  1,   0,  12,
  86, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  87, "PRESSURE",                            "Pa", -1,   0,  14,
  88, "TEMPERATURE/DRY BULB TEMPERATURE",     "K",  1,   0,  12,
  89, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  90, "PRESSURE",                            "Pa", -1,   0,  14,
  91, "TEMPERATURE/DRY BULB TEMPERATURE",     "K",  1,   0,  12,
  92, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  93, "PRESSURE",                            "Pa", -1,   0,  14,
  94, "TEMPERATURE/DRY BULB TEMPERATURE",     "K",  1,   0,  12,
  95, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  96, "PRESSURE",                            "Pa", -1,   0,  14,
  97, "TEMPERATURE/DRY BULB TEMPERATURE",     "K",  1,   0,  12,
  98, "HEIGHT ASSIGNMENT METHOD",           "C/T",  0,   0,   4,
  99, "PRESSURE",                            "Pa", -1,   0,  14,
  100, "TEMPERATURE/DRY BULB TEMPERATURE",    "K",  1,   0,  12,
  101, "HEIGHT ASSIGNMENT METHOD",          "C/T",  0,   0,   4,
  102, "PRESSURE",                           "Pa", -1,   0,  14,
  103, "TEMPERATURE/DRY BULB TEMPERATURE",    "K",  1,   0,  12,
/*****************
 222000 uuu, "QUALITY INFORMATION FOLLOWS", "N/A",  0,   0,   6,
 236000 vvv, "DEFINE BIT-MAP",              "N/A",  0,   0,   6,
*****************/
  104, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  105, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  106, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  107, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  108, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  109, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  110, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  111, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  112, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  113, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  114, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  115, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  116, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  117, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  118, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  119, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  120, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  121, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  122, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  123, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  124, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  125, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  126, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  127, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  128, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  129, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  130, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  131, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  132, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  133, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  134, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  135, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  136, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  137, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  138, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  139, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  140, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  141, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  142, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  143, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  144, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  145, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  146, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  147, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  148, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  149, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  150, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  151, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  152, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  153, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  154, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  155, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  156, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  157, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  158, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  159, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  160, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  161, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  162, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  163, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  164, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  165, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  166, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  167, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  168, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  169, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  170, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  171, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  172, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  173, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  174, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  175, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  176, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  177, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  178, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  179, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  180, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  181, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  182, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  183, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  184, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  185, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  186, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  187, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  188, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  189, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  190, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  191, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  192, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  193, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  194, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  195, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  196, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  197, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  198, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  199, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  200, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  201, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  202, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  203, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  204, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  205, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  206, "DATA PRESENT INDICATOR",            "N/A",  0,   0,   1,
  207, "IDENTIFICATION OF ORIG/GEN CENTRE", "C/T",  0,   0,  16,
  208, "GENERATING APPLICATION",            "C/T",  0,   0,   8,
  209, "% CONFIDENCE",                        "%",  0,   0,   7,
  210, "% CONFIDENCE",                        "%",  0,   0,   7,
  211, "% CONFIDENCE",                        "%",  0,   0,   7,
  212, "% CONFIDENCE",                        "%",  0,   0,   7,
  213, "% CONFIDENCE",                        "%",  0,   0,   7,
  214, "% CONFIDENCE",                        "%",  0,   0,   7,
  215, "% CONFIDENCE",                        "%",  0,   0,   7,
  216, "% CONFIDENCE",                        "%",  0,   0,   7,
  217, "% CONFIDENCE",                        "%",  0,   0,   7,
  218, "% CONFIDENCE",                        "%",  0,   0,   7,
/*****************
 222000 www, "QUALITY INFORMATION FOLLOWS", "N/A",  0,   0,   6,
 237000 xxx, "REUSE PREV DEFINED BIT-MAP",  "N/A",  0,   0,   6,
*****************/
  219, "IDENTIFICATION OF ORIG/GEN CENTRE", "C/T",  0,   0,  16,
  220, "GENERATING APPLICATION",            "C/T",  0,   0,   8,
  221, "% CONFIDENCE",                        "%",  0,   0,   7,
  222, "% CONFIDENCE",                        "%",  0,   0,   7,
  223, "% CONFIDENCE",                        "%",  0,   0,   7,
  224, "% CONFIDENCE",                        "%",  0,   0,   7,
  225, "% CONFIDENCE",                        "%",  0,   0,   7,
  226, "% CONFIDENCE",                        "%",  0,   0,   7,
  227, "% CONFIDENCE",                        "%",  0,   0,   7,
  228, "% CONFIDENCE",                        "%",  0,   0,   7,
  229, "% CONFIDENCE",                        "%",  0,   0,   7,
  230, "% CONFIDENCE",                        "%",  0,   0,   7,
/*****************
 222000 yyy, "QUALITY INFORMATION FOLLOWS", "N/A",  0,   0,   6,
 237000 zzz, "REUSE PREV DEFINED BIT-MAP",  "N/A",  0,   0,   6,
*****************/
  231, "IDENTIFICATION OF ORIG/GEN CENTRE", "C/T",  0,   0,  16,
  232, "GENERATING APPLICATION",            "C/T",  0,   0,   8,
  233, "% CONFIDENCE",                        "%",  0,   0,   7,
  234, "% CONFIDENCE",                        "%",  0,   0,   7,
  235, "% CONFIDENCE",                        "%",  0,   0,   7,
  236, "% CONFIDENCE",                        "%",  0,   0,   7,
  237, "% CONFIDENCE",                        "%",  0,   0,   7,
  238, "% CONFIDENCE",                        "%",  0,   0,   7,
  239, "% CONFIDENCE",                        "%",  0,   0,   7,
  240, "% CONFIDENCE",                        "%",  0,   0,   7,
  241, "% CONFIDENCE",                        "%",  0,   0,   7,
  242, "% CONFIDENCE",                        "%",  0,   0,   7
};


struct WindHeader
{ 
  short int shSatID;
  short int shGenCen;
  short int shSatClass;
  short int shSatInst;
  float fXSegSize;
  float fYSegSize;
  short int shYear;
  short int shMonth;
  short int shDay;
  short int shHour;
  short int shMin;
  short int shSec;
  short int shWMethod;
  float fFreq;
  float fWidth;
};

struct WindQC
{ 
  float fPercConfRFI;
  float fPercConfRFF;
  float fPercConfEUM;
};

struct WindComponent
{
  short int shTimeSigSt;
  short int shHourSt;
  short int shMinSt;
  short int shSecSt;
  short int shTimeSigEnd;
  short int shHourEnd;
  short int shMinEnd;
  short int shSecEnd;
  float fDirection;
  struct WindQC stDirnQC;
  float fSpeed;
  struct WindQC stSpeedQC;
};

struct WindHeight
{ 
  short int shMethod;
  float fPressure;
  float fTemperature;
};

struct WindData
{ 
  float fLat;
  float fLon;
  float fPress;
  float fDirn;
  struct WindQC stDirnQC;
  float fSpeed;
  struct WindQC stSpeedQC;
  float fTemp;
  short int shHMethod;
  short int shTMethod;
  short int shLandSea;
  float fZenith;
  short int shFOrig;
  short int shTimeSig1;
  short int shYear;
  short int shMonth;
  short int shDay;
  short int shHour;
  short int shTimeSig2;
  short int shTimePer;
  struct WindComponent stComp[4];
  struct WindHeight stHeight[10];
};

  FILE *fpInputFile;
  struct stat stFileStat;
  char *pzOneBulletin;
  char *pzFormat;
  struct WindData *psWinds;
  struct WindHeader sHeader;
  long int liFileSize;
  long int liBulLen;
  long int liLoop;
  long int liSect1Offset, liSect1Lenght;
  long int liSect2Offset, liSect2Lenght;
  long int liSect3Offset, liSect3Lenght;
  long int liSect4Offset, liSect4Lenght;
  long int liSect5Offset, liSect5Lenght;
  long int liNumberN, liNumberO, liNumberP, liNumberQ;
  long int liNumberR, liNumberS, liNumberT, liNumberU;
  long int liTotSubSets;
  long int liSubSet;
  unsigned long int uliWorkBits;
  unsigned long long ullToilBits;
  short int shEdition;
  short int shMasterTab;
  short int shGenSubCen;
  short int shGenCentre;
  short int shUpdateSeq;
  short int shOptnSect2;
  short int shDataCateg;
  short int shSubCateg;
  short int shMastrVers;
  short int shLocalVers;
  short int shYearOfCen;
  short int shMonth;
  short int shDay;
  short int shHour;
  short int shMinute;
  short int shObAndComp;
  short int shCompress;
  short int shSect3F, shSect3X, shSect3Y;
  short int shNProblems = 0;
  short int shElement;
  long int liR0;
  float fR0, fValue;
  short int shNBINC;
  long int liStartByte;
  short int shStartBit;
  long int liEndByte;
  short int shEndBit;
  long int liByteInc, liOrigByte;
  short int shBitDec, shOrigBit;
  static char *stpzMonth[] = {"DummyShift","January","February","March","April",
    "May","June","July","August","September","October","November","December"};
  if (0==iBulletin) (void)fprintf(Out_File, "\n===============\nBUFR MESSAGE NUMBER: %d\n=========\n", N_message+1);
  /* Section 0 */
  if (0==iBulletin) (void)fprintf(Out_File, "\nSection 0\n=========\n");

  pzFormat = (char *)malloc((size_t)5);
  (void)memcpy(pzFormat, pzWholeFile, 4);
  *(pzFormat + 4) = '\0';
  if (0==iBulletin) (void)fprintf(Out_File, "Data format: %s\n",pzFormat); 

  liNumberS = *(pzWholeFile + 4); unsign(liNumberS);
  liNumberT = *(pzWholeFile + 5); unsign(liNumberT);
  liNumberU = *(pzWholeFile + 6); unsign(liNumberU);
  liBulLen = (liNumberS * 256 * 256) + (liNumberT * 256) + liNumberU;
  if (0==iBulletin) (void)fprintf(Out_File, "Total message length: %d\n", liBulLen);

  shEdition = *(pzWholeFile + 7); unsign(shEdition);
  if (0==iBulletin) (void)fprintf(Out_File, "BUFR Edition number: %d\n", shEdition);

  liSect1Offset = 8;

  /* Section 1 */
  if (0==iBulletin) (void)fprintf(Out_File, "\nSection 1\n=========\n");

  liNumberS = *(pzWholeFile + liSect1Offset); unsign(liNumberS);
  liNumberT = *(pzWholeFile + liSect1Offset + 1); unsign(liNumberT);
  liNumberU = *(pzWholeFile + liSect1Offset + 2); unsign(liNumberU);
  liSect1Lenght = (liNumberS * 256 * 256) + (liNumberT * 256) + liNumberU;
  if (0==iBulletin) (void)fprintf(Out_File, "Length of section: %d\n", liSect1Lenght);

  shMasterTab = *(pzWholeFile + liSect1Offset + 3); unsign(shMasterTab);
 if (0==iBulletin) (void)fprintf(Out_File, "BUFR master table: %d\n", shMasterTab);

  shGenSubCen = *(pzWholeFile + liSect1Offset + 4); unsign(shGenSubCen);
 if (0==iBulletin) (void)fprintf(Out_File, "Generatin sub-centre: %d\n", shGenSubCen);

  shGenCentre = *(pzWholeFile + liSect1Offset + 5); unsign(shGenCentre);
 if (0==iBulletin) (void)fprintf(Out_File, "Generating centre: %d%s\n", shGenCentre,
    (254==shGenCentre)?" [EUMETSAT]":" ");

  shUpdateSeq = *(pzWholeFile + liSect1Offset + 6); unsign(shUpdateSeq);
 if (0==iBulletin) (void)fprintf(Out_File, "Undate sequence number: %d\n", shUpdateSeq);

  shOptnSect2 = *(pzWholeFile + liSect1Offset + 7); unsign(shOptnSect2);
  shOptnSect2 = (shOptnSect2&0200)>>7;
 if (0==iBulletin) (void)fprintf(Out_File, "Section 2 indicator: %s.\n",shOptnSect2?"Present":"Omitted");

  shDataCateg = *(pzWholeFile + liSect1Offset + 8); unsign(shDataCateg);
 if (0==iBulletin) (void)fprintf(Out_File, "Data category: %d\n", shDataCateg);

  shSubCateg = *(pzWholeFile + liSect1Offset + 9); unsign(shSubCateg);
 if (0==iBulletin) (void)fprintf(Out_File, "Data sub-category: %d\n", shSubCateg);

  shMastrVers = *(pzWholeFile + liSect1Offset + 10); unsign(shMastrVers);
 if (0==iBulletin) (void)fprintf(Out_File, "Master table version: %d\n", shMastrVers);

  shLocalVers = *(pzWholeFile + liSect1Offset + 11); unsign(shLocalVers);
 if (0==iBulletin) (void)fprintf(Out_File, "Local table version: %d\n", shLocalVers);

  shYearOfCen = *(pzWholeFile + liSect1Offset + 12); unsign(shYearOfCen);
 if (0==iBulletin) (void)fprintf(Out_File, "Year of century: %d\n", shYearOfCen);

  shMonth = *(pzWholeFile + liSect1Offset + 13); unsign(shMonth);

  shDay = *(pzWholeFile + liSect1Offset + 14); unsign(shDay);

  shHour = *(pzWholeFile + liSect1Offset + 15); unsign(shHour);

  shMinute = *(pzWholeFile + liSect1Offset + 16); unsign(shMinute);

 if (0==iBulletin) (void)fprintf(Out_File, "Date: %s %2.2d\n", stpzMonth[shMonth], shDay);
 if (0==iBulletin) (void)fprintf(Out_File, "Time: %2.2d:%2.2d\n", shHour, shMinute);

  liSect2Offset = (liSect1Offset + liSect1Lenght);

  /* Section 2 */
  if (!shOptnSect2)
  {
    liSect2Lenght = 0;
  }
  else
  {
    if (0==iBulletin) (void)fprintf(Out_File, "\nSection 2\n=========\n");
    liNumberS = *(pzWholeFile + liSect2Offset); unsign(liNumberS);
    liNumberT = *(pzWholeFile + liSect2Offset + 1); unsign(liNumberT);
    liNumberU = *(pzWholeFile + liSect2Offset + 2); unsign(liNumberU);
    liSect2Lenght = (liNumberS * 256 * 256) + (liNumberT * 256) + liNumberU;
    if (0==iBulletin) (void)fprintf(Out_File, "Length of section: %d\n", liSect2Lenght);
  }

  liSect3Offset = (liSect2Offset + liSect2Lenght);

  /* Section 3 */
  if (0==iBulletin) (void)fprintf(Out_File, "\nSection 3\n=========\n");

  liNumberS = *(pzWholeFile + liSect3Offset); unsign(liNumberS);
  liNumberT = *(pzWholeFile + liSect3Offset + 1); unsign(liNumberT);
  liNumberU = *(pzWholeFile + liSect3Offset + 2); unsign(liNumberU);
  liSect3Lenght = (liNumberS * 256 * 256) + (liNumberT * 256) + liNumberU;
  if (0==iBulletin) (void)fprintf(Out_File, "Length of section: %d\n", liSect3Lenght);

  liNumberT = *(pzWholeFile + liSect3Offset + 4); unsign(liNumberT);
  liNumberU = *(pzWholeFile + liSect3Offset + 5); unsign(liNumberU);
  liTotSubSets = (liNumberT * 256) + liNumberU;
  if (0==iBulletin) (void)fprintf(Out_File, "Number of subsets: %d\n", liTotSubSets);

  shObAndComp = *(pzWholeFile + liSect3Offset + 6); unsign(shObAndComp);
  if (((shObAndComp&0200)>>7) == 1)
  {
    if (0==iBulletin) (void)fprintf(Out_File, "Data content: Message contains observation data.\n");
  }
  else
  {
    if (0==iBulletin) (void)fprintf(Out_File, "Data content: Message contains \"other\" data.\n");
  }
  shCompress = (shObAndComp&0100)>>6;
  if (0==iBulletin) (void)fprintf(Out_File, "Compression status: Data are %s.\n",
    shCompress?"compressed":"not compressed");

  if (0==iBulletin) (void)fprintf(Out_File, "Descriptor list\n");
  for (liLoop = 1; liLoop <= ((liSect3Lenght - 8) / 2); liLoop++)
  {
    liNumberT = *(pzWholeFile + liSect3Offset + 7 + ((liLoop - 1) * 2));
    unsign(liNumberT);
    shSect3F = (liNumberT&0300)>>6;
    shSect3X = liNumberT&0077;
    liNumberU = *(pzWholeFile + liSect3Offset + 7 + ((liLoop - 1) * 2) + 1);
    unsign(liNumberU );
    shSect3Y = liNumberU;
    if (0==iBulletin) (void)fprintf(Out_File, "%1d-%2.2d-%3.3d ", shSect3F, shSect3X, shSect3Y);
    switch (shSect3F)
    {
      case 0:
        switch (shSect3X)
        {
          case 1:
            switch (shSect3Y)
            {
              case 7:
                if (0==iBulletin) (void)fprintf(Out_File, "Satellite identifiier.\n");
                break;
              case 31:
                if (0==iBulletin) (void)fprintf(Out_File, "Generating centre.\n");
                break;
              case 32:
                if (0==iBulletin) (void)fprintf(Out_File, "Generating sub-centre.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 2:
            switch (shSect3Y)
            {
              case 20:
                if (0==iBulletin) (void)fprintf(Out_File, "Satellite classification.\n");
                break;
              case 28:
                if (0==iBulletin) (void)fprintf(Out_File, "Segment size (X).\n");
                break;
              case 29:
                if (0==iBulletin) (void)fprintf(Out_File, "Segment size (Y).\n");
                break;
              case 152:
                if (0==iBulletin) (void)fprintf(Out_File, "Satellite instrument.\n");
                break;
              case 153:
                if (0==iBulletin) (void)fprintf(Out_File, "Channel frequency.\n");
                break;
              case 154:
                if (0==iBulletin) (void)fprintf(Out_File, "Channel width.\n");
                break;
              case 166:
                if (0==iBulletin) (void)fprintf(Out_File, "Radiance type.\n");
                break;
              case 167:
                if (0==iBulletin) (void)fprintf(Out_File, "Radiance compn method.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 4:
            switch (shSect3Y)
            {
              case 1:
                if (0==iBulletin) (void)fprintf(Out_File, "Year.\n");
                break;
              case 2:
                if (0==iBulletin) (void)fprintf(Out_File, "Month.\n");
                break;
              case 3:
                if (0==iBulletin) (void)fprintf(Out_File, "Day.\n");
                break;
              case 4:
                if (0==iBulletin) (void)fprintf(Out_File, "Hour.\n");
                break;
              case 5:
                if (0==iBulletin) (void)fprintf(Out_File, "Minute.\n");
                break;
              case 6:
                if (0==iBulletin) (void)fprintf(Out_File, "Second.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 5:
            switch (shSect3Y)
            {
              case 1:
                if (0==iBulletin) (void)fprintf(Out_File, "Fine accuracy latitude.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 6:
            switch (shSect3Y)
            {
              case 1:
                if (0==iBulletin) (void)fprintf(Out_File, "Fine accuracy longitude.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 7:
            switch (shSect3Y)
            {
              case 24:
                if (0==iBulletin) (void)fprintf(Out_File, "Satellite zenith angle.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 8:
            switch (shSect3Y)
            {
              case 12:
                if (0==iBulletin) (void)fprintf(Out_File, "Land/Sea qualifier.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 12:
            switch (shSect3Y)
            {
              case 63:
                if (0==iBulletin) (void)fprintf(Out_File, "Brightness temperature.\n");
                break;
              case 75:
                if (0==iBulletin) (void)fprintf(Out_File, "Spectral radiance.\n");
                break;
              case 76:
                if (0==iBulletin) (void)fprintf(Out_File, "Radinace.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 20:
            switch (shSect3Y)
            {
              case 81:
                if (0==iBulletin) (void)fprintf(Out_File, "Amount of cloud.\n");
                break;
              case 82:
                if (0==iBulletin) (void)fprintf(Out_File, "Amount cloud free.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          case 31:
            if (31 == shSect3Y)
            {
              if (0==iBulletin) (void)fprintf(Out_File, "Data present indicator.\n");
            }
            else
            {
              if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
              shNProblems++;
            }
            break;
          case 33:
            switch (shSect3Y)
            {
              case 7:
                if (0==iBulletin) (void)fprintf(Out_File, "Percentage confidence.\n");
                break;
              case 35:
                if (0==iBulletin) (void)fprintf(Out_File, "Quality control method.\n");
                break;
              case 36:
                if (0==iBulletin) (void)fprintf(Out_File, "Nominal confidence threshold.\n");
                break;
              default:
                if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
                shNProblems++;
                break;
            }
            break;
          default:
            if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised element descriptor.\n");
            shNProblems++;
            break;
        }
        break;
      case 1:
        if (0==iBulletin) (void)fprintf(Out_File, "Replicate %d %s %d times.\n", shSect3X, 
          (shSect3X > 1)?"descriptors":"descriptor", shSect3Y);
        break;
      case 2:
        if (0 == shSect3Y)
        {
          switch (shSect3X)
          {
            case 22:
              if (0==iBulletin) (void)fprintf(Out_File, "Quality control information follows.\n");
              break;
            case 36:
              if (0==iBulletin) (void)fprintf(Out_File, "Data present bit map follows.\n");
              break;
            case 37:
              if (0==iBulletin) (void)fprintf(Out_File, "Re-use previously defined bit map.\n");
              break;
            default:
              if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised operator descriptor.\n");
              shNProblems++;
              break;
          }
        }
        else
        {
          if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised operator descriptor.\n");
          shNProblems++;
        }
        break;
      case 3:
        if ((10 == shSect3X) && (14 == shSect3Y))
        {
          if (0==iBulletin) (void)fprintf(Out_File, "Geostationary satellite wind sequence.\n");
        }
        else
        {
          if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised sequence descriptor.\n");
          shNProblems++;
        }
        break;
      default:
        if (0==iBulletin) (void)fprintf(Out_File, "***ALERT***: Unrecognised descriptor type.\n");
        shNProblems++;
        break;
    }  
  }

  liSect4Offset = (liSect3Offset + liSect3Lenght);

  /* Section 4 */
  if (0==iBulletin) (void)fprintf(Out_File, "\nSection 4\n=========\n");

  liNumberS = *(pzWholeFile + liSect4Offset); unsign(liNumberS);
  liNumberT = *(pzWholeFile + liSect4Offset + 1); unsign(liNumberT);
  liNumberU = *(pzWholeFile + liSect4Offset + 2); unsign(liNumberU);
  liSect4Lenght = (liNumberS * 256 * 256) + (liNumberT * 256) + liNumberU;
  if (0==iBulletin) (void)fprintf(Out_File, "Length of section: %d\n", liSect4Lenght);

  if ((shCompress) && (0 == shNProblems))
  {
/* Allocate dynamic memory for winds */
    psWinds = (struct WindData *)
      malloc((size_t)(sizeof(struct WindData) * liTotSubSets));
/* Initialise byte and bit offsets */
    liStartByte = liSect4Offset + 4;
    shStartBit = 1;
    liEndByte = liSect4Offset + 4;
    shEndBit = 1;
    for (shElement = 0; shElement < NDESCS; shElement++)
    {
/* Read reference value (allow up to 32 bits) */
      liNumberN = *(pzWholeFile + liStartByte); unsign(liNumberN);
      liNumberO = *(pzWholeFile + liStartByte + 1); unsign(liNumberO);
      liNumberP = *(pzWholeFile + liStartByte + 2); unsign(liNumberP);
      liNumberQ = *(pzWholeFile + liStartByte + 3); unsign(liNumberQ);
      liNumberR = *(pzWholeFile + liStartByte + 4); unsign(liNumberR);
      liNumberS = *(pzWholeFile + liStartByte + 5); unsign(liNumberS);
      liNumberT = *(pzWholeFile + liStartByte + 6); unsign(liNumberT);
      liNumberU = *(pzWholeFile + liStartByte + 7); unsign(liNumberU);

      ullToilBits = ((unsigned long long)liNumberN * 256 * 256 * 256 * 256 * 256 * 256 * 256) +
                    ((unsigned long long)liNumberO * 256 * 256 * 256 * 256 * 256 * 256) +
                    ((unsigned long long)liNumberP * 256 * 256 * 256 * 256 * 256) +
                    ((unsigned long long)liNumberQ * 256 * 256 * 256 * 256) +
                    ((unsigned long long)liNumberR * 256 * 256 * 256) +
                    ((unsigned long long)liNumberS * 256 * 256) +
                    ((unsigned long long)liNumberT * 256) +
                     (unsigned long long)liNumberU;
      ullToilBits >>= (64 - ((shStartBit + sDescs[shElement].shWidth) - 1));
      ullToilBits = ullToilBits &
        ((unsigned long int)
          pow((double)2, (double)(sDescs[shElement].shWidth)) - 1);
      liR0 = ullToilBits;
      fR0 = (float)liR0;
/* Calculate end byte and bit */
      liEndByte = ((liStartByte * 8) + shStartBit +
        (sDescs[shElement].shWidth - 1)) / 8;
      shEndBit = ((liStartByte * 8) + shStartBit +
        (sDescs[shElement].shWidth - 1)) % 8;
      if (0 == shEndBit)
      {
        shEndBit = 8;
        liEndByte -= 1;
      }
/* Calculate next start byte and bit */
      liStartByte = liEndByte;
      shStartBit = shEndBit + 1;
      if (9 == shStartBit)
      {
        shStartBit = 1;
        liStartByte += 1;
      }
/* Read number of bits per descriptor used (always 6 bits) */
      liNumberT = *(pzWholeFile + liStartByte); unsign(liNumberT);
      liNumberU = *(pzWholeFile + liStartByte + 1); unsign(liNumberU);
      uliWorkBits = ((unsigned long int)liNumberT * 256) +
                     (unsigned long int)liNumberU;
      uliWorkBits >>= (16 - ((shStartBit + 6) - 1));
      uliWorkBits = uliWorkBits &
        ((unsigned long int)pow((double)2, (double)(6)) - 1);
      shNBINC = (short int)uliWorkBits;
/* Calculate end byte and bit */
      liEndByte = ((liStartByte * 8) + shStartBit + (6 - 1)) / 8;
      shEndBit = ((liStartByte * 8) + shStartBit + (6 - 1)) % 8;
      if (0 == shEndBit)
      {
        shEndBit = 8;
        liEndByte -= 1;
      }
/* Calculate next start byte and bit */
      liStartByte = liEndByte;
      shStartBit = shEndBit + 1;
      if (9 == shStartBit)
      {
        shStartBit = 1;
        liStartByte += 1;
      }
/* Display some info */
      if (0==iBulletin) (void)fprintf(Out_File, "%36s (%4s) Ref: %ld, Bits used: %d\n",
        sDescs[shElement].pzName, sDescs[shElement].pzUnits, liR0, shNBINC);
/* If the bits per incr is 0, all values are the same */
      if (0 == shNBINC)
      {
/* Start with reference value from compression */
        fValue = (float)(liR0);
/* Add reference value from Table B */
        fValue += (float)(sDescs[shElement].liRef);
/* Apply scale from Table B */
        fValue /= (float)pow((double)10, (double)(sDescs[shElement].shScale));
/* Fill in header/static info or the value for one one subset*/
        switch (sDescs[shElement].shIndex)
        {
        case 1:
          sHeader.shSatID = (short int)fValue;
          break;
        case 2:
          sHeader.shGenCen = (short int)fValue;
          break;
        case 3:
          sHeader.shSatClass = (short int)fValue;
          break;
        case 4:
          sHeader.fXSegSize = fValue;
          break;
        case 5:
          sHeader.fYSegSize = fValue;
          break;
        case 6:
          sHeader.shYear = (short int)fValue;
          break;
        case 7:
          sHeader.shMonth = (short int)fValue;
          break;
        case 8:
          sHeader.shDay = (short int)fValue;
          break;
        case 9:
          sHeader.shHour = (short int)fValue;
          break;
        case 10:
          sHeader.shMin = (short int)fValue;
          break;
        case 11:
          sHeader.shSec = (short int)fValue;
          break;
        case 14:
          sHeader.shSatInst = (short int)fValue;
          break;
        case 15:
          sHeader.shWMethod = (short int)fValue;
          break;
        case 19:
          sHeader.fFreq = fValue;
          break;
        case 20:
          sHeader.fWidth = fValue;
          break;
        default:
          break; 
        }

        if (1==liTotSubSets) {
        switch (sDescs[shElement].shIndex)
        {
            case 12:
              psWinds[0].fLat = fValue;
              break;
            case 13:
              psWinds[0].fLon = fValue;
              break;
            case 16:
              psWinds[0].fPress = fValue;
              break;
            case 17:
              psWinds[0].fDirn = fValue;
              break;
            case 18:
              psWinds[0].fSpeed = fValue;
              break;
            case 21:
              psWinds[0].fTemp = fValue;
              break;
            case 25:
              psWinds[0].fZenith = fValue;
              break;
            case 209:
              psWinds[0].stDirnQC.fPercConfRFI = fValue;
              break;
            case 210:
              psWinds[0].stSpeedQC.fPercConfRFI = fValue;
              break;
            case 221:
              psWinds[0].stDirnQC.fPercConfRFF = fValue;
              break;
            case 222:
              psWinds[0].stSpeedQC.fPercConfRFF = fValue;
              break;
            case 233:
              psWinds[0].stDirnQC.fPercConfEUM = fValue;
              break;
            case 234:
              psWinds[0].stSpeedQC.fPercConfEUM = fValue;
              break;
            }
          }
      }
/* Otherwise read all the values */
      else
      {
        for (liSubSet = 0; liSubSet < liTotSubSets; liSubSet++)
        {
/* Read reference value (allow up to 32 bits) */
          liNumberN = *(pzWholeFile + liStartByte); unsign(liNumberN);
          liNumberO = *(pzWholeFile + liStartByte + 1); unsign(liNumberO);
          liNumberP = *(pzWholeFile + liStartByte + 2); unsign(liNumberP);
          liNumberQ = *(pzWholeFile + liStartByte + 3); unsign(liNumberQ);
          liNumberR = *(pzWholeFile + liStartByte + 4); unsign(liNumberR);
          liNumberS = *(pzWholeFile + liStartByte + 5); unsign(liNumberS);
          liNumberT = *(pzWholeFile + liStartByte + 6); unsign(liNumberT);
          liNumberU = *(pzWholeFile + liStartByte + 7); unsign(liNumberU);

          ullToilBits = ((unsigned long long)liNumberN * 256 * 256 * 256 * 256 * 256 * 256 * 256) +
                        ((unsigned long long)liNumberO * 256 * 256 * 256 * 256 * 256 * 256) +
                        ((unsigned long long)liNumberP * 256 * 256 * 256 * 256 * 256) +
                        ((unsigned long long)liNumberQ * 256 * 256 * 256 * 256) +
                        ((unsigned long long)liNumberR * 256 * 256 * 256) +
                        ((unsigned long long)liNumberS * 256 * 256) +
                        ((unsigned long long)liNumberT * 256) +
                         (unsigned long long)liNumberU;
          ullToilBits >>= (64 - ((shStartBit + shNBINC) - 1));
          ullToilBits = ullToilBits &
            ((unsigned long int)pow((double)2, (double)(shNBINC)) - 1);
/* Add reference value from compression */
          fValue = (float)(ullToilBits + liR0);
/* Add reference value from Table B */
          fValue += (float)(sDescs[shElement].liRef);
/* Apply scale from Table B */
          fValue /= (float)pow((double)10, (double)(sDescs[shElement].shScale));
/* Fill wind info */
          switch (sDescs[shElement].shIndex)
          {
            case 12:
              psWinds[liSubSet].fLat = fValue;
              break;
            case 13:
              psWinds[liSubSet].fLon = fValue;
              break;
            case 16:
              psWinds[liSubSet].fPress = fValue;
              break;
            case 17:
              psWinds[liSubSet].fDirn = fValue;
              break;
            case 18:
              psWinds[liSubSet].fSpeed = fValue;
              break;
            case 21:
              psWinds[liSubSet].fTemp = fValue;
              break;
            case 25:
              psWinds[liSubSet].fZenith = fValue;
              break;
            case 209:
              psWinds[liSubSet].stDirnQC.fPercConfRFI = fValue;
              break;
            case 210:
              psWinds[liSubSet].stSpeedQC.fPercConfRFI = fValue;
              break;
            case 221:
              psWinds[liSubSet].stDirnQC.fPercConfRFF = fValue;
              break;
            case 222:
              psWinds[liSubSet].stSpeedQC.fPercConfRFF = fValue;
              break;
            case 233:
              psWinds[liSubSet].stDirnQC.fPercConfEUM = fValue;
              break;
            case 234:
              psWinds[liSubSet].stSpeedQC.fPercConfEUM = fValue;
              break;
            default:
              break; 
          }
/* Calculate end byte and bit */
          liEndByte = ((liStartByte * 8) + shStartBit + (shNBINC - 1)) / 8;
          shEndBit = ((liStartByte * 8) + shStartBit + (shNBINC - 1)) % 8;
          if (0 == shEndBit)
          {
            shEndBit = 8;
            liEndByte -= 1;
          }
/* Calculate next start byte and bit */
          liStartByte = liEndByte;
          shStartBit = shEndBit + 1;
          if (9 == shStartBit)
          {
            shStartBit = 1;
            liStartByte += 1;
          }
        }
      }
    }
  }
  else
  {
    if (0==iBulletin) (void)fprintf(Out_File, "Section 4 decoding not supported for these data.\n");
  }

  liSect5Offset = (liSect4Offset + liSect4Lenght);

  /* Section 5 */
  if (0==iBulletin) (void)fprintf(Out_File, "\nSection 5\n=========\n");

  (void)memcpy(pzFormat, pzWholeFile + liSect5Offset, 4);
  *(pzFormat + 4) = '\0';
  if (0==iBulletin) (void)fprintf(Out_File, "End sequence: %s\n",pzFormat); 

  /* Print out header */
  if (0==iBulletin) (void)fprintf(Out_File, "\nHeader data\n===========\n");
  if (0==iBulletin) (void)fprintf(Out_File, "Satellite ID         %d\n",sHeader.shSatID);
  if (0==iBulletin) (void)fprintf(Out_File, "Generating Centre    %d\n",sHeader.shGenCen);
  if (0==iBulletin) (void)fprintf(Out_File, "Satellite Class      %d\n",sHeader.shSatClass);
  if (0==iBulletin) (void)fprintf(Out_File, "Satellite Instrument %d\n",sHeader.shSatInst);
  if (0==iBulletin) (void)fprintf(Out_File, "Satellite Derived Wind %d\n",sHeader.shWMethod);
  if (0==iBulletin) (void)fprintf(Out_File, "Satellite Channel Center Frequency %10.5e (Hz)\n",sHeader.fFreq);
  if (0==iBulletin) (void)fprintf(Out_File, "Satellite Channel Band Width %10.5e (Hz)\n",sHeader.fWidth);
  if (0==iBulletin) (void)fprintf(Out_File, "Segment size X dirn  %10.3e (m)\n",sHeader.fXSegSize);
  if (0==iBulletin) (void)fprintf(Out_File, "Segment size Y dirn  %10.3e (m)\n",sHeader.fYSegSize);
  if (0==iBulletin) (void)fprintf(Out_File, "Date: %s %2.2d, %4.4d\n", stpzMonth[sHeader.shMonth],
    sHeader.shDay, sHeader.shYear);
  if (0==iBulletin) (void)fprintf(Out_File, "Time: %2.2d:%2.2d:%2.2d\n", sHeader.shHour, sHeader.shMin,
    sHeader.shSec);

  /* Print out wind data */
  if (0==iBulletin) (void)fprintf(Out_File, "\nVector data:\n");
  if ((0==N_message) || (0==iBulletin)) (void)fprintf(Out_File, "Lat, Lon, Press, Dir, Speed, Temp, Zenith, DirQCRFI, SpeedQCRFI,DirQCRFF, SpeedQCRFF,DirQCEUM, SpeedQCEUM\n");
  if (0==iBulletin) (void)fprintf(Out_File, "\n===========\n");
  for (liSubSet = 0; liSubSet < liTotSubSets; liSubSet++)
  {
     fprintf(Out_File, "%6.2f %6.2f %7.1f %6.2f %6.2f %6.2f %6.2f ",
      psWinds[liSubSet].fLat,
      psWinds[liSubSet].fLon,
      psWinds[liSubSet].fPress,
      psWinds[liSubSet].fDirn,
      psWinds[liSubSet].fSpeed,
      psWinds[liSubSet].fTemp,
      psWinds[liSubSet].fZenith);
     fprintf(Out_File, "%5.1f %5.1f %5.1f %5.1f %5.1f %5.1f\n",
      psWinds[liSubSet].stDirnQC.fPercConfRFI,
      psWinds[liSubSet].stSpeedQC.fPercConfRFI,
      psWinds[liSubSet].stDirnQC.fPercConfRFF,
      psWinds[liSubSet].stSpeedQC.fPercConfRFF,
      psWinds[liSubSet].stDirnQC.fPercConfEUM,
      psWinds[liSubSet].stSpeedQC.fPercConfEUM);
  }

  /* Free up dynamic memory */
  free((void *)pzFormat);
  if (NULL != psWinds) free((void *)psWinds);
   }
