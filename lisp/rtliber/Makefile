# Copyright (C) 2014  Yoni Rabkin (yrk@gnu.org)

# This file is part of RT Liberation.

# RT Liberation is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 2 of the
# License, or (at your option) any later version.

# RT Liberation is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with RT Liberation.  If not, see
# <http://www.gnu.org/licenses/>.

EMACS=emacs
ALLSOURCE=$(wildcard *.el)
ALLCOMPILED=$(wildcard *.elc)
SPECIAL=maint.el
SOURCE=$(filter-out $(SPECIAL),$(ALLSOURCE))
TARGET=$(patsubst %.el,%.elc,$(SOURCE))

.PHONY: all clean
.PRECIOUS: %.elc
all: $(TARGET)

%.elc: %.el
	@$(EMACS) -q -batch \
		-l maint.el \
		-f batch-byte-compile $<

clean:
	-rm -f *~ *.elc
