# CL-BLIKY

## Description

cl-bliky is a very simple blogging engine written in lisp.

It's based on a [tutorial](http://roeim.net/vetle/docs/cl-webapp-intro/) 
written by [Vetle Roeim](http://roeim.net/vetle/). 

Posts are written, or uploaded using a web browser.
The posts are inter-linked static html pages.
The engine is configured to push the pages to a [github pages](http://pages.github.com) repo.
The engine is easily configured so that the generated page can be served directly 
from a server like apache or nginx as well.


## dependencies
cl-bliky depends on the following packages :

* [elephant](http://common-lisp.net/project/elephant/) 
  with [berkley db](http://www.oracle.com/technology/products/berkeley-db/index.html).
* [hunchentoot](http://www.weitz.de/hunchentoot/)
* [html-template](http://www.weitz.de/html-template/)

## initial startup

