# Copyright 2014 Hamish Ivey-Law
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


def parse_desc(fname):
    fp = open(fname, 'r')
    headers = email.Parser().parse(fp)
    fp.close()


if __name__ == "__main__":
    parse_desc(sys.argv[1])
