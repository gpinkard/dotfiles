### Gabe's ACME Colors

This is the [ACME editor](https://en.wikipedia.org/wiki/Acme_(text_editor)) with a few changes I made to the colorscheme. The new colors are defined in **colors.h**, and used in **acme.c** (see the **iconinit** function).

I use [plan9port](https://github.com/9fans/plan9port) to get ACME. Clone the repository, then put **acme.c** and **colors.h** into **$PLAN9/src/cmd/acme/** (overriding the original acme.c file). Then run the **./INSTALL** script.

![Here is how it looks](acme.png)