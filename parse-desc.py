
def parse_desc(fname):
    fp = open(fname, 'r')
    headers = email.Parser().parse(fp)
    fp.close()


if __name__ == "__main__":
    parse_desc(sys.argv[1])
