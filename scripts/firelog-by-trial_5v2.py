#! /usr/bin/env python
#Script to read csv files of downloaded HOBO firelogger data and calculate
# summary statistics
# 2011-01-21

import string, sys, os, time
import logging
from datetime import datetime, timedelta
from numpy import argmax
from scipy.interpolate import interp1d


MONTHS = {"Oct":10,"Nov":11,"Dec":12}

logging.basicConfig(format='\n%(levelname)s:\n%(message)s\n')
firelog_logger = logging.getLogger('firelog_logger')

__docformat__ = "restructuredtext en"
__authors__  =   "Dylan W. Schwilk"
__version__ = "0.2"
__needs__ = '2.4'

#TRIAL_TIME_FORMAT = "%m/%d/%y %I:%M %p"
TRIAL_TIME_FORMAT = "%H:%M"
HOBO_TIME_FORMAT  = "%d/%m/%y %I:%M:%S %p"

TEMP_FLOOR = 60    # for celsius

# values outside these ranges will be considered 'NA' (errors)
MIN_TEMP = 0.0
MAX_TEMP = 900.0

TIME_EPOCH = datetime(2010,1,1)  # for peak temperature time outputs in seconds from epoch

def read_flatfile(file, delim=","):
    """Usage: read(filename,delim=None) Returns: a dictionary of lists
    from delimited text file.  The first line is assumed to be a
    header containing column. Numbers that can be converted to floats
    are so converted
"""

    if not delim :
        spl = string.split
    else :
        spl = lambda x : x.split(delim)
        
    file = open(file)
    lines = map(spl, file.readlines())
#    headers = "ID","Time","Temp","Started","HostConnected","Stopped","EndOfFile"
    headers = ["ID","Time","Temp"]
#    headers = lines[1]  # top line is title
#    map(lambda x : x.strip(),headers)
    ncols = len(headers)
    result = {} # result is a dictionary accessed by column name
    for i in range(ncols):
        result[headers[i]] = []  # add empty list

       
    
    for j, line in enumerate(lines[2:]):
        for i in range(ncols):
            try:
                 result[headers[i]].append(string.atoi(line[i]))
            except ValueError:
                try:
                   result[headers[i]].append(string.atof(line[i]))
                except:
                    result[headers[i]].append(line[i])

    # convert to celsius and convert times
    #result["Temp"] = map(lambda x: (5.0/9.0)*(x-32), result["Temp"])
    # convert times from format 04/29/08 12:36:33 PM
    result["Time"] = map(lambda x: datetime.strptime(x,HOBO_TIME_FORMAT), result["Time"])
    return result


def get_start_stop_indices(hobo,starttime,stoptime):
   # print starttime, stoptime
   # start = min( hobo["Time"], key = lambda date : abs(starttime-date))
    # for i,t in enumerate(hobo["Time"]):
    #     if t > starttime:
    #         start = i
    #         break
            
    # for i,t in enumerate(hobo["Time"][start:]):
    #     if t > stoptime:
    #         stop = i+start
    #         break
    thedate = hobo["Time"][0]  # time file does not use dates, find correct date from hobo file
    stime = datetime(thedate.year,thedate.month,thedate.day,starttime.hour,starttime.minute)
    etime = datetime(thedate.year,thedate.month,thedate.day,stoptime.hour,stoptime.minute)
    start = hobo["Time"].index(stime)
    stop = hobo["Time"].index(etime)
    #print start,stop
    return start,stop
    
def get_peak_temp_time(hobo, start, stop):
    """ Get peak temperature and time of peak . Returned time as  number of seconds since TIME_EPOCH"""
    i = argmax(hobo["Temp"][start:stop])
    td = hobo["Time"][start:stop][i] - TIME_EPOCH
    pt = (td.microseconds + (td.seconds + td.days * 24 * 3600) * 10**6) / 10**6
    return hobo["Temp"][start:stop][i], pt


def get_duration_heat(hobo, start, stop, tempfloor):
    """Get time temp crosses floor, seconds above floor and total heat (degmin)
    return as tuple"""
    #alltimes = []
    heat = 0.0
    numsecs = 0
    temptime=0
    firsttemp = False
    for i, t in enumerate(hobo["Temp"][start:stop]):
        if t > tempfloor:
            if not firsttemp:
                td = hobo["Time"][start:stop][i] - TIME_EPOCH
                temptime = (td.microseconds + (td.seconds + td.days * 24 * 3600) * 10**6) / 10**6
                #temptime = hobo["Time"][start:stop][i]
                firsttemp = True
                
            #alltimes.append(hobo["Time"][start:stop][i])
            heat += (1.0/60.0) * t
            numsecs+=1
    if numsecs < 1 : return (-1,0,0)
    #begin = min(alltimes)
    #end = max(alltimes)
    return(temptime,numsecs, heat)
                         
def clean_hobodata(hobo):
    "Remove error values"
    fixer = interp1d(hobo["ID"],hobo["Temp"])
    for i, t in enumerate(hobo["Temp"]):
        if t < MIN_TEMP or t > MAX_TEMP:
            #print("\nfixing value " + str(t) + "ID " + str(hobo["ID"][i]))
            hobo["Temp"][i] = float(fixer(hobo["ID"][i]))
            

def write_hobo_slice(hobo,start,stop,fname):
    "Write out temperature timeseries to a file"
    f = open(fname,"w")
    f.write("s,time,temp\n")
    for i, sec in enumerate(hobo["ID"][start:stop]):
        f.write("%i,%s,%d\n" % (sec, hobo["Time"][start:stop][i], hobo["Temp"][start:stop][i]))

    f.close()

    
def main():
    """Command-line tool. See firelog.py -h for help."""

    import sys
    #from collections import OrderedDict
    #from tools import list_files

    try:
        from optparse import OptionParser
    except (ImportError, AttributeError):
        try:
            from optik import OptionParser
        except (ImportError, AttributeError):
            print """firelogger program needs python 2.3 or Greg Ward's optik module."""
    
    usage = "usage: %prog [options] [hobo files (csv)]"
    parser = OptionParser(usage=usage, version ="%prog " + __version__)
    parser.add_option("-d", "--datfiles", action="store_true",
                      dest="datfile", default=0,
                      help="Create timeseries data file for each trial and sensor")
    parser.add_option("-t", "--timefile", action="store", dest="timefile", default="",
					  help="Specify file with trial burn times",metavar="FILE")    
    (options, args) = parser.parse_args()

    # process all files/directories given
    if len(args)<1 :
        print "No firelogger csv files given. Usage: %s [options] [photo_files]" % os.path.basename(sys.argv[0])
        sys.exit(1)

    if options.timefile:
        trials = []
        tf = open(options.timefile)
        for l in tf.readlines():
            strippy = lambda x : x.strip('"\n\t ')
            datestr, b, spcode, start,stop =  map(strippy, l.split(','))
            ms,d = datestr.split("-")
            mo = MONTHS[ms]
            datestr = "2010-%d-%s" % ( mo, d)
            starts  = datetime.strptime(start, TRIAL_TIME_FORMAT)
            stops   = datetime.strptime(stop , TRIAL_TIME_FORMAT)            
            trials.append( (datestr, b, spcode, starts, stops))  
        
    files= []

    for i in args[0:]:
        if os.path.isfile(i): files.append(i)
 

    #Now get dictionary indexed by file name

    firedata = []
    files.sort()

    print "date,trial,spcode",
    
    # get all hobo data into a dictionary by filename
    for hobofile in files:
        hobodata = read_flatfile(hobofile)
        clean_hobodata(hobodata)
        hname = os.path.basename(hobofile)[0:-4]
        hname = hname.split("_")[0]
        firedata.append((hname,hobodata))
        print ",%s,%s,%s,%s,%s,%s,%s,%s" % (hname + "." + "ptemp", hname + "." + "peaktemptime", hname + "." + "time100", hname + "." +"numsecs100",  hname + "." +"heat100", hname + "time60", hname + "." +"numsecs60",  hname + "." +"heat60") ,
    print "\n",

    for trial in trials:
        dstr, b, spcode, s, e = trial
        print(dstr + "," +  b + "," + spcode),
        for (logger, hdata) in firedata:
            start,stop = get_start_stop_indices(hdata, s, e)
            ptemp, ptime = get_peak_temp_time(hdata, start,stop)
            time60, numsecs60, heat60 = get_duration_heat(hdata, start,stop,60)
            time100, numsecs100, heat100 = get_duration_heat(hdata, start,stop,100)
            print ",%f,%d,%d,%d,%d,%d,%d,%d" % (ptemp, ptime, time100, numsecs100,  heat100, time60, numsecs60, heat60) ,
            if options.datfile:
                write_hobo_slice(hdata,start,stop, spcode + "_" + b + "_" + dstr + "_" + logger + ".dat")
        print "\n",

             
# Main program
if __name__ == "__main__" :
    main()
