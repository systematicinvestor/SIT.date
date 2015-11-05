---
layout: post
title: SIT.date - Wrappers for Common Date Operations
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




SIT.date package is the collection of functions to ease extraction of date period ends.

The first function that i want to highlight is the `date.ends` function 
is user friendly wrapper to compute periods ends with optional logic to handle
holidays if holiday calendar is provided. It handles various commonly used periodicity by mapping them
into corresponding end period functions. For example, weeks, week, weekly, and w are all mapped into `date.week.ends'.

It also handles `bi-weekly` syntax by taking every other period end. For more precise handling you can directly 
specify by and skip input parameters. 

Let's checkout some examples:


{% highlight r %}
library(SIT.date)

dates = seq(as.Date('1-Oct-2013','%d-%b-%Y'), as.Date('10-Jan-2015','%d-%b-%Y'), 1)

dates[date.ends(dates, 'bi-monthly')]
{% endhighlight %}

[1] "2013-10-31" "2013-12-31" "2014-02-28" "2014-04-30" "2014-06-30"
[6] "2014-08-31" "2014-10-31" "2014-12-31"


{% highlight r %}
dates[date.ends(dates, 'bi-monthly', skip=1)]
{% endhighlight %}

[1] "2013-11-30" "2014-01-31" "2014-03-31" "2014-05-31" "2014-07-31"
[6] "2014-09-30" "2014-11-30" "2015-01-10"


{% highlight r %}
dates[date.ends(dates, 'monthly', skip=1, by=2)]
{% endhighlight %}

[1] "2013-11-30" "2014-01-31" "2014-03-31" "2014-05-31" "2014-07-31"
[6] "2014-09-30" "2014-11-30" "2015-01-10"


{% highlight r %}
dates = seq(as.Date('1-Mar-2015','%d-%b-%Y'), as.Date('1-May-2015','%d-%b-%Y'), 1)

dates[date.ends(dates, 'weekly', calendar = 'UnitedStates/NYSE')]
{% endhighlight %}

[1] "2015-03-07" "2015-03-14" "2015-03-21" "2015-03-28" "2015-04-04"
[6] "2015-04-11" "2015-04-18" "2015-04-25" "2015-05-01"


{% highlight r %}
dates[date.ends(dates, 'weekly', last.date=F, calendar = 'Canada/TSX')]
{% endhighlight %}

[1] "2015-03-07" "2015-03-14" "2015-03-21" "2015-03-28" "2015-04-04"
[6] "2015-04-11" "2015-04-18" "2015-04-25" "2015-05-01"


{% highlight r %}
# Good Friday 	April 3, 2015-04-03 is holiday
dates[date.ends(dates, 'weekly')]
{% endhighlight %}

[1] "2015-03-07" "2015-03-14" "2015-03-21" "2015-03-28" "2015-04-04"
[6] "2015-04-11" "2015-04-18" "2015-04-25" "2015-05-01"


Another function that you might find handy is the `custom.date.bus` and `custom.date` functions.
These functions handle custom syntax to specify date ends. The expression given to these functions must
be in the following format:

([identifier] [periodicity]) [in/every] ([identifier] [periodicity]) ... [in/every] ([identifier] [periodicity])

For example:

* last day in Apr
* last day in first week in Apr
* 3rd Mon in last M in Q1
* 10th to last day in Apr

The `custom.date.bus` function also takes in the holiday calendar to properly adjust for holidays.

Let's checkout some examples:



{% highlight r %}
library(SIT.date)

dates = seq(as.Date('1-Oct-2010','%d-%b-%Y'), as.Date('1-Jan-2015','%d-%b-%Y'), 1)

dates[custom.date('last day in Apr', dates)]
{% endhighlight %}

[1] "2011-04-30" "2012-04-30" "2013-04-30" "2014-04-30"


{% highlight r %}
dates[custom.date.bus('last day in Apr', dates, 'UnitedStates/NYSE')]
{% endhighlight %}

[1] "2011-04-29" "2012-04-30" "2013-04-30" "2014-04-30"


{% highlight r %}
dates = seq(as.Date('1-Oct-2014','%d-%b-%Y'), as.Date('1-Jan-2015','%d-%b-%Y'), 1)

dates[custom.date.bus('first day in Jan', dates, 'UnitedStates/NYSE')]
{% endhighlight %}

character(0)


{% highlight r %}
dates = seq(as.Date('1-Jan-2014','%d-%b-%Y'), as.Date('5-Jan-2015','%d-%b-%Y'), 1)
dates[custom.date('first day in Jan', dates)]
{% endhighlight %}

[1] "2014-01-01" "2015-01-01"


{% highlight r %}
dates[custom.date.bus('first day in Jan', dates, 'UnitedStates/NYSE')]
{% endhighlight %}

[1] "2014-01-02" "2015-01-02"


{% highlight r %}
# with out calendar the, logic assumes that 1 jan 2014 is first business date
dates[custom.date.bus('first day in Jan', dates)]
{% endhighlight %}

[1] "2014-01-01" "2015-01-01"


{% highlight r %}
dates[custom.date('last day in Apr', dates)]
{% endhighlight %}

[1] "2014-04-30"


{% highlight r %}
dates[custom.date('first day in Apr', dates)]
{% endhighlight %}

[1] "2014-04-01"


{% highlight r %}
dates[custom.date('last day in first week in Apr', dates)]
{% endhighlight %}

[1] "2014-04-05"


{% highlight r %}
dates[custom.date('last Mon in Apr', dates)]
{% endhighlight %}

[1] "2014-04-28"


{% highlight r %}
dates[custom.date('last Fri in Apr', dates)]
{% endhighlight %}

[1] "2014-04-25"


{% highlight r %}
dates[custom.date('first day in Apr', dates)]
{% endhighlight %}

[1] "2014-04-01"


{% highlight r %}
dates[custom.date('1st day in Apr', dates)]
{% endhighlight %}

[1] "2014-04-01"


{% highlight r %}
dates[custom.date('10th day in Apr', dates)]
{% endhighlight %}

[1] "2014-04-10"


{% highlight r %}
dates[custom.date('50th day in Apr', dates)]
{% endhighlight %}

[1] "2014-04-30"


{% highlight r %}
dates[custom.date('10th to last day in Apr', dates)]
{% endhighlight %}

[1] "2014-04-20"


{% highlight r %}
dates[custom.date('3rd Mon in Q', dates)]
{% endhighlight %}

[1] "2014-01-20" "2014-04-21" "2014-07-21" "2014-10-20" "2015-01-05"


{% highlight r %}
dates[custom.date('3rd Mon in 1st Q', dates)]
{% endhighlight %}

[1] "2014-01-20"


{% highlight r %}
dates[custom.date('3rd Mon in Q1', dates)]
{% endhighlight %}

[1] "2014-01-20" "2015-01-05"


{% highlight r %}
dates[custom.date('3rd Mon in last M in Q1', dates)]
{% endhighlight %}

[1] "2014-03-17" "2015-01-05"


{% highlight r %}
# Options Expiration is third Friday of the expiration month
# the expiration months are the first month of each quarter - January, April, July, October. 
dates[custom.date('3rd Fri in Q', dates)]
{% endhighlight %}

[1] "2014-01-17" "2014-04-18" "2014-07-18" "2014-10-17" "2015-01-02"


{% highlight r %}
dates = seq(as.Date('1-Jan-2010','%d-%b-%Y'), as.Date('29-Apr-2015','%d-%b-%Y'), 1)
dates[custom.date('last day in Apr', dates)]
{% endhighlight %}

[1] "2010-04-30" "2011-04-30" "2012-04-30" "2013-04-30" "2014-04-30"
[6] "2015-04-29"


{% highlight r %}
dates = seq(as.Date('1-Jan-2010','%d-%b-%Y'), as.Date('30-Apr-2015','%d-%b-%Y'), 1)
dates[custom.date('last day in Apr', dates)]
{% endhighlight %}

[1] "2010-04-30" "2011-04-30" "2012-04-30" "2013-04-30" "2014-04-30"
[6] "2015-04-30"


{% highlight r %}
dates = seq(as.Date('1-Jan-2010','%d-%b-%Y'), as.Date('29-Apr-2015','%d-%b-%Y'), 1)
dates[custom.date.bus('last day in Apr', dates)]
{% endhighlight %}

[1] "2010-04-30" "2011-04-29" "2012-04-30" "2013-04-30" "2014-04-30"

Please let me know if you run into any bugs.

The [SIT.date]() is available at 
Please install with `install_github('systematicinvestor/SIT.date')`


*(this report was produced on: 2015-11-05)*
