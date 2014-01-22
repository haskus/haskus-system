# ViperVM TODO list #

## OpenCL ##

### Bypass the ICD (Installable Client Driver) ###

We should use the native implementations directly as we already use dynamic
linking and because the ICD is crap (bad support for different OpenCL versions,
etc.). We should support more than a single OpenCL library in the platform
configuration.

We could simulate the ICD behavior if necessary to automatically find clients
(using ICD files). In addition we could support the OCL_ICD_VENDORS
environnement variable (path to ICD files). 

Not very difficult to do. 

References:

* [http://www.khronos.org/registry/cl/extensions/khr/cl_khr_icd.txt]
* [https://forge.imag.fr/projects/ocl-icd/]
