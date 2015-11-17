

###############################################################################
#' Dates Functions
#'
#' @param dates collection of dates
#'
#' @return transformed dates
#'
#' @examples
#' \dontrun{ 
#' date.dayofweek(Sys.Date())
#' }
#' @export 
#' @rdname DateFunctions
###############################################################################
date.dayofweek <- function(dates) { 
  as.POSIXlt(dates)$wday
}


#' @export 
#' @rdname DateFunctions
date.day <- function(dates) { 
  as.POSIXlt(dates)$mday
}


# wday 0-6 day of the week, starting on Sunday.
# %U Week of the year as decimal number (00-53) using Sunday as the first day 1 of the week (and typically with the first Sunday of the year as day 1 of week 1). The US convention.
#' @export 
#' @rdname DateFunctions
date.week <- function(dates) { 
	dates = as.POSIXlt(dates)
	offset = (7 + dates$wday - dates$yday %% 7) %%7
	(dates$yday +  offset)%/% 7
}

 
#' @export 
#' @rdname DateFunctions
date.month <- function(dates) { 
	as.POSIXlt(dates)$mon + 1
}


quarter.map = c(1,1,1,2,2,2,3,3,3,4,4,4)

#' @export 
#' @rdname DateFunctions
date.quarter <- function(dates) { 	
  quarter.map[date.month(dates)] 
}


semiannual.map = c(1,1,1,1,1,1,2,2,2,2,2,2)

#' @export 
#' @rdname DateFunctions
date.semiannual = function (dates) {
	semiannual.map[date.month(dates)] 
}


#' @export 
#' @rdname DateFunctions
date.year = function (dates) {
	as.POSIXlt(dates)$year + 1900
}


#' @export 
#' @rdname DateFunctions
date.all = function(dates) 
{
	dates = as.POSIXlt(dates)
	offset = (7 + dates$wday - dates$yday %% 7) %%7

	list(
		dayofweek = dates$wday,
		mday = dates$mday,
		yday = dates$yday,
		weeks = (dates$yday +  offset)%/% 7,
		months = dates$mon + 1,		
		quarters = quarter.map[dates$mon + 1],
		semiannual = semiannual.map[dates$mon + 1],
		years = dates$year + 1900	
	)
}


# Test above functionality
date.period.test = function() {
	# test date functions
	# ?DateTimeClasses
	# ?strptime 
	date.dayofweek0 <- function(dates) { 
	  return(as.double(format(dates, '%w')))
	}
	date.day0 <- function(dates) { 
	  return(as.double(format(dates, '%d')))
	}
	date.week0 <- function(dates) { 
	  return(as.double(format(dates, '%U')))
	}
	date.month0 <- function(dates) { 
		return(as.double(format(dates, '%m')))
	}
	# (((1:12)-1) %/% 3)+1  
	date.quarter0 <- function(dates) { 	
		(((date.month(dates))-1) %/% 3)+1
	}
	date.year0 = function (dates) {
		return(as.double(format(dates, '%Y')))
	}
	

	#------------------------------------------

	dates1 = seq(Sys.Date()-100000, Sys.Date(), 1)
	all.equal(diff(date.week0(dates1))!=0 , diff(date.week(dates1))!=0 )
	
	dates = seq(Sys.Date()-10000, Sys.Date(), 1)

	library(rbenchmark)
	benchmark(
    	test1 = diff(date.week0(dates))!=0, 
       	test2 = diff(date.week(dates))!=0, 
       	columns = c("test", "replications", "elapsed", "relative"),
       	order = "relative",
       	replications = 200
   	)

   	#------------------------------------------
   	
	all.equal(diff(date.dayofweek0(dates1))!=0 , diff(date.dayofweek(dates1))!=0 )
   		
	benchmark(
    	test1 = diff(date.dayofweek0(dates))!=0, 
       	test2 = diff(date.dayofweek(dates))!=0, 
       	columns = c("test", "replications", "elapsed", "relative"),
       	order = "relative",
       	replications = 200
   	)
   	
	#------------------------------------------
   	
	all.equal(diff(date.day0(dates1))!=0 , diff(date.day(dates1))!=0 )
   	   	
	benchmark(
    	test1 = diff(date.day0(dates))!=0, 
       	test2 = diff(date.day(dates))!=0, 
       	columns = c("test", "replications", "elapsed", "relative"),
       	order = "relative",
       	replications = 200
   	)   	
   	
	#------------------------------------------
   	
	all.equal(diff(date.month0(dates1))!=0 , diff(date.month(dates1))!=0 )
   	   	
	benchmark(
    	test1 = diff(date.month0(dates))!=0, 
       	test2 = diff(date.month(dates))!=0, 
       	columns = c("test", "replications", "elapsed", "relative"),
       	order = "relative",
       	replications = 200
   	)   	
   	
	#------------------------------------------
   	
	all.equal(diff(date.quarter0(dates1))!=0 , diff(date.quarter(dates1))!=0 )
   	
	benchmark(
    	test1 = diff(date.quarter0(dates))!=0, 
       	test2 = diff(date.quarter(dates))!=0, 
       	columns = c("test", "replications", "elapsed", "relative"),
       	order = "relative",
       	replications = 200
   	)   	

	#------------------------------------------
 
	all.equal(diff(date.year0(dates1))!=0 , diff(date.year(dates1))!=0 )
   	
	benchmark(
    	test1 = diff(date.year0(dates))!=0, 
       	test2 = diff(date.year(dates))!=0, 
       	columns = c("test", "replications", "elapsed", "relative"),
       	order = "relative",
       	replications = 200
   	)   	   	  	
}


###############################################################################
#' Dates Index Functions
#'
#' @param dates collection of dates
#' @param last.date flag to include last date, \strong{defaults to TRUE}
#'
#' @return location of the week/month/year ends
#'
#' @examples
#' \dontrun{ 
#' date.week.ends(seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
#' @rdname DateFunctionsIndex
###############################################################################
date.week.ends <- function(dates, last.date=T) 
{ 
  ends = which(diff( 100*date.year(dates) + date.week(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}


#' @export 
#' @rdname DateFunctionsIndex
date.month.ends <- function(dates, last.date=T) 
{ 
  ends = which(diff( 100*date.year(dates) + date.month(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}


#' @export 
#' @rdname DateFunctionsIndex
date.quarter.ends <- function(dates, last.date=T) 
{ 
  ends = which(diff( 10*date.year(dates) + date.quarter(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}


#' @export 
#' @rdname DateFunctionsIndex
date.semiannual.ends = function(dates, last.date=T) 
{ 
  ends = which(diff( 10*date.year(dates) + date.semiannual(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}


#' @export 
#' @rdname DateFunctionsIndex
date.year.ends <- function(dates, last.date=T) 
{ 
  ends = which(diff( date.year(dates) ) != 0)
  ends.add.last.date(ends, len(dates), last.date)
}


# helper function to add last date
ends.add.last.date = function(ends, last.date, action=T) 
{
  if(action)
    unique(c(ends, last.date))
  else
    ends
}


###############################################################################
#' Dates Mapping Functions
#'
#' @param periodicity periodicity (i.e. weeks, months)
#'
#' @return standard periodicity
#'
#' @examples
#' \dontrun{ 
#' date.freq.map('week')
#' }
#' @export 
#' @rdname DateFunctionsMap
###############################################################################
date.periodicity.map = function(periodicity) {
  switch(periodicity,
	days = 'days',
    day = 'days',
    daily = 'days',
	d = 'days',
  
    weeks = 'weeks',
    week = 'weeks',
    weekly = 'weeks',
	w = 'weeks',
    
    months = 'months',
    month = 'months',
    monthly = 'months',
	m = 'months',
    
    quarters = 'quarters',
    quarter = 'quarters',
    quarterly = 'quarters',
	q = 'quarters',

	semiannual = 'semiannual',
	semiannually = 'semiannual',
	s = 'semiannual',
		
    years = 'years',
    year = 'years',
    yearly = 'years',
    annual = 'years',
    annually = 'years',
	y = 'year',
    
    # default
    NULL)  
}

#' @export 
#' @rdname DateFunctionsMap
date.ends.fn = function(periodicity) {
  switch(date.periodicity.map(periodicity),
    weeks = date.week.ends,
    months = date.month.ends,
    quarters = date.quarter.ends,
	semiannual = date.semiannual.ends,
    years = date.year.ends,
    
    # default
    NULL)  
}


###############################################################################
#' Apply Business Date logic to provided Dates function 
#'
#' @param dates dates, dates are assumed to contain only business dates
#' @param dates.fn dates function that returns index of selected dates, \strong{defaults to NULL}
#' @param calendar RQuantLib calendar name to use to determine business days, \strong{defaults to NULL}
#'
#' @return index of business days
#'
#' @examples
#' \dontrun{ 
#' dates = seq(Sys.Date()-1000, Sys.Date(), 1)
#' apply.business.days(dates, date.week.ends, 'UnitedStates/NYSE')
#' apply.business.days(dates, date.week.ends, 'Canada/TSX')
#' }
#' @export 
###############################################################################
apply.business.days = function(dates, dates.fn = NULL, calendar = NULL, base = T) {
	# assume that if xts is given; it is sourced from historical data and
	# does not contain holidays; we just need to check boundary cases
	if( xts::is.xts(dates) ) {
		dates = index(dates)
		apply.business.days.internal(dates, dates.fn, calendar)
	} else {
		ok.index = business.days(dates = dates, calendar = calendar, return.index=T)	
		index = apply.business.days.internal(dates[ok.index], dates.fn, calendar)
		(1:len(dates))[ok.index][index]
	}
}

apply.business.days.internal = function(dates, dates.fn = NULL, calendar = NULL, base = T) {
	if( xts::is.xts(dates) ) dates = index(dates)
	dates = as.Date(dates)
		n = len(dates)

	# getHolidayList	
	holidays = NULL
	if( !is.null(calendar) ) {	  	
		if( requireNamespace('RQuantLib', quietly = T) )
			holidays = RQuantLib::getHolidayList(calendar, dates[1] - 60, dates[1] - 1)
		else
			warning('RQuantLib could not be loaded')
	}
	before = business.days(dates[1] - 60, dates[1] - 1, holidays)
		n.before = len(before) 

	if( !is.null(holidays) ) 
		holidays = RQuantLib::getHolidayList(calendar, dates[n] + 1, dates[n] + 60)   
	after = business.days(dates[n] + 1, dates[n] + 60, holidays)

	dates = c(before, dates, after)
	
	if( !is.null(dates.fn) ) 
		index = dates.fn(dates)
	else
		index = 1:len(dates)

	# map back to original dates
	if( base ) {
		index = index[index > n.before & index <= (n.before + n)]
			index = index - n.before
		return(index)
	}
	
	# till / since logic (business.days.location.end)
	original.dates.index = (n.before + 1) : (n.before + n)
	temp.cum = cumsum(rep(1, len(dates)))
		temp = temp.cum * NA
		temp[index] = temp.cum[index]
	days.since = temp.cum - ifna.prev(temp)
	days.till = temp[ifna.prevx.rev(temp)] - temp.cum
	list(days.since = days.since[original.dates.index], days.till = days.till[original.dates.index])	
}




###############################################################################
#' Business Date Since / Till function 
#'
#' @param dates dates
#' @param dates.fn dates function that returns index of selected dates, \strong{defaults to date.month.ends}
#' @param calendar RQuantLib calendar name to use to determine business days, \strong{defaults to NULL}
#'
#' @return list with days.since and days.till arrays
#'
#' @examples
#' \dontrun{ 
#' dates = seq(Sys.Date()-1000, Sys.Date(), 1)
#' business.days.location.end(dates, date.week.ends, 'UnitedStates/NYSE')
#' business.days.location.end(dates, date.week.ends, 'Canada/TSX')
#' }
#' @export 
###############################################################################
business.days.location.end = function(dates, dates.fn = date.month.ends, calendar = NULL) { 
	apply.business.days(dates, dates.fn, calendar, F)
}


#' out is result of the business.days.location.end
#' @export 
date.ends.index <- function(out, timing) {
  if(timing <= 0)
    which(out$days.till == (-timing))
  else
    which(out$days.since == (timing))
}


###############################################################################
#' Date Ends function 
#'
#' @param dates dates
#' @param periodicity periodicity
#' @param by take every *by* item, \strong{defaults to 1}
#' @param skip skip periods, \strong{defaults to 0}
#' @param last.date flag to include last date, \strong{defaults to TRUE}
#' @param calendar RQuantLib calendar name to use to determine business days, \strong{defaults to NULL}
#'
#' @return index of date ends
#'
#' @examples
#' \dontrun{ 
#' dates = seq(Sys.Date()-1000, Sys.Date(), 1)
#' date.ends(dates, 'year', calendar = 'UnitedStates/NYSE')
#' date.ends(dates, 'year', calendar = 'Canada/TSX')
#' date.ends(dates, 'bi-weekly')
#' }
#' @export 
###############################################################################
date.ends = function(dates, periodicity, by=1, skip=0, last.date=T, calendar = NULL) {
	periodicity = trim(tolower(periodicity))
	
	# bi- means 'every two', as in every two [weeks/months/years, etc]
	# biweekly = every two weeks / bimonthly = every two months
	# semi- to mean 'twice every' (as semiannually - twice per year)
	# semiweekly = twice a week / semimonthly = twice a month
	bi.flag = substr(periodicity,1,2) == 'bi'
	#semi.flag = substr(periodicity,1,4) == 'semi'	

	if(bi.flag) periodicity = substr(periodicity,3,1000)
		by = if(bi.flag) 2 else by
	#if(semi.flag) periodicity = substr(periodicity,5,1000)
	periodicity = trim(gsub('-','',periodicity))
		
	ends = apply.business.days(dates, calendar = calendar,
		dates.fn = function(x) {			
			fn = date.ends.fn(periodicity)
			if( is.null(fn) )
				xts::endpoints(xts::xts(1:len(x), x), periodicity)			
			else
				fn(x, last.date=F)		
		})

	if( xts::is.xts(dates) ) dates = index(dates)		
	ends = ends.add.last.date(ends, len(dates), last.date)		
				
	if( skip > 0) ends = ends[-c(1:skip)]
	if( by > 1) ends = ends[seq(1, len(ends), by=by)]

	ends		
}

#' last calendar day of period
#' date.end('2014-01-13')
#' @export 
#' @rdname DateFunctionsIndex 
date.end <- function(date = Sys.Date(), periodicity = 'months', date.format = '%Y-%m-%d') {
  date = as.Date(paste(date), date.format)
  temp = seq(date, date + 40, 1)
  temp[date.ends.fn(periodicity)(temp)[1]]
}


###############################################################################
#' Extract Business Days
#'
#' @param from start date (ignored if dates are provided)
#' @param to start date (ignored / optional if dates are provided)
#' @param holidays list of holidays, \strong{defaults to NULL}
#' @param dates dates, \strong{defaults to NULL}
#' @param calendar RQuantLib calendar name to use to determine business days, \strong{defaults to NULL}
#' @param return.index flag to return index of valid dates instead of dates, \strong{defaults to FALSE}
#'
#' @return business days
#'
#' @examples
#' \dontrun{ 
#' library(RQuantLib)
#' from = as.Date('10Jun2013','%d%b%Y')
#' to = as.Date('10Jan2014','%d%b%Y')
#' holidays = getHolidayList('UnitedStates/NYSE', from, to)  
#' holidays = getHolidayList('Canada/TSX', from, to)  
#' business.days(from, to, holidays = holidays)
#'
#' business.days(dates = dates, calendar = 'UnitedStates/NYSE')
#' }
#' @export 
###############################################################################
business.days = function(
	from = Sys.Date(), 
	to = as.Date(from) + 31, 
	holidays = NULL, 
	dates = NULL, 
	calendar = NULL,
	return.index = F
) {
	if( is.null(dates) ) 
		dates = seq(as.Date(from), as.Date(to), 1)
	else if( xts::is.xts(dates) ) 
		dates = index(dates)	
		
    rm.index = date.dayofweek(dates) == 6 | date.dayofweek(dates) == 0

	# getHolidayList	
	if( !is.null(calendar) ) {	  	
		if( requireNamespace('RQuantLib', quietly = T) )
			holidays = RQuantLib::getHolidayList(calendar, dates[1], dates[len(dates)])
		else
			warning('RQuantLib could not be loaded')
	}
	
    if( !is.null(holidays) ) {
        holidays = as.Date(holidays)
        rm.index = rm.index | !is.na(match(dates, holidays))        
    }
	
	if( return.index )
		!rm.index
	else
		dates[!rm.index]
}


###############################################################################
#' Business Date Functions
#'
#' @param from start date
#' @param holidays list of holidays, \strong{defaults to NULL}
#' @param fn.ends function that return periods ends, \strong{defaults to date.month.ends}
#'
#' @return number of business days
#'
#' @examples
#' \dontrun{ 
#' from = as.Date('27Dec2013','%d%b%Y')
#' holidays = getHolidayList('UnitedStates/NYSE', from-40, from+10)  
#' business.days.till.end(from, holidays)
#' business.days.since.end(from, holidays)
#' }
#' @export 
#' @rdname BusinessDateFunctions
###############################################################################
business.days.till.end <- function(from, holidays = NULL, fn.ends = date.month.ends) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business.days(from - 10, from, holidays)
  from = dates[len(dates)]
  
  dates = business.days(from, from + 40, holidays)
  index = match.fun(fn.ends)(dates, F)
  index[1] - 1
}


#' @export 
#' @rdname BusinessDateFunctions
business.days.since.end <- function(from, holidays = NULL, fn.ends = date.month.ends) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business.days(from - 10, from, holidays)
  from = dates[len(dates)]
    
  dates = business.days(from - 40, from + 10, holidays)
  index = match.fun(fn.ends)(dates, F)
      
  last.index = index[len(index)]
  if( dates[last.index] == from) return(0)
  
  from.index = sum(dates <= from)
  if( dates[last.index] < from) return(from.index - last.index)
  
  last.index = index[(len(index) - 1)]
  return(from.index - last.index)
}


#' @export 
#' @rdname BusinessDateFunctions
next.business.day <- function(from, holidays = NULL, offset = 0) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business.days(from + offset, from + 10, holidays)
  dates[1]
}


#' @export last.business.day
#' @rdname BusinessDateFunctions
last.business.day <- function(from, holidays = NULL, offset = 0) {
  from = as.Date(from)
  
  # make sure from is a business date
  dates = business.days(from - 10, from - offset, holidays)
  dates[1]
}



###############################################################################
#' Compute the expiration date of stock options (3rd Friday of the month)
#'
#' @param year year
#' @param month month
#'
#' @return date for the third Friday of the given month and year
#'
#' @references 
#' \url{http://bytes.com/topic/python/answers/161147-find-day-week-month-year}
#'
#' \url{http://www.mysmp.com/options/options-expiration-week.html}
#' The week beginning on Monday prior to the Saturday of options expiration is referred to as options expiration week. 
#' Since the markets are closed on Saturday, the third Friday of each month represents options expiration.
#' If the third Friday of the month is a holiday, all trading dates are moved forward; meaning that Thursday will be the last trading day to exercise options.
#'
#' \url{http://www.cboe.com/TradTool/ExpirationCalendar.aspx}
#'
#' @examples
#' \dontrun{ 
#' third.friday.month(2012,1)
#' }
#' @export 
###############################################################################
third.friday.month <- function(years, months)
{ 
helper <- function(year, month) {
  day = date.dayofweek( as.Date(c('', 10000*year + 100*month + 1), '%Y%m%d')[-1] )
  day = c(20,19,18,17,16,15,21)[1 + day]
  as.Date(c('', 10000*year + 100*month + day), '%Y%m%d')[-1]
}
  if(len(years) > 1 && len(months) > 1) {
    out = c()
    for(month in months)
      out = c(out, helper(years,month))
    as.Date(out)
  } else
    helper(years,months) 
} 

# Special days
# http://www.cboe.com/AboutCBOE/xcal2014.pdf
# third.friday.month(2010:2013, 4)  
# key.date = map.spx.expiration(data$prices)  
# VIX settles 30 days prior to SPY
# key.date = map.spx.expiration(data$prices, offset=30)   
# na.omit(key.date['2014'])
#' @export
map.spx.expiration <- function(data, backfill = T, offset = 0) {
  dates = as.Date(index(data))
  
  # 3rd Friday of the month is last trading day for equity options
  years = date.year(range(dates))
  friday = third.friday.month(years[1]:(years[2]+1), 1:12)
    friday.future = friday[friday > dates[len(dates)]]
    friday = friday[friday <= dates[len(dates)]]
  
  key.date.index = match(friday, dates)
  na.index = which(is.na(key.date.index))
  
  # backfill NA's
  if(backfill && len(na.index)>0)
    key.date.index[na.index] = match(friday[na.index]-1, dates)

  if(offset != 0) {
    friday = c(dates[key.date.index], friday.future)
    offset.date = friday - offset
    key.date.index = match(offset.date, dates)
  }
  
  key.date.index = na.omit(key.date.index)
  
  key.date = NA * data[,1]
    key.date[key.date.index,] = T
  key.date
}


