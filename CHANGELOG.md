# Changelog

## v0.1.0-dev (2015-04-10)

* **License Change** - Garth Hitchens (original copyright holder), formally changed license to MIT as part of an open source release.   Previous license text was:
  
        Copyright © 2012-2013 Garth Hitchens, All Rights Reserved
        Repurposed technology and design from the RemoteRadio Project, 
        RemoteRadio is Copyright © 1996-2012 Garth Hitchens, KG7GA

* Significant Code Changes

  * updated to use rest_init function to allow callbacks on cowboy 1.0
  * added on_wait_start, on_wait_end, response_hook callbacks and tests
  * removed dependencies on other hub modules
  * cleaned up atomize/1 and deatomize/1
