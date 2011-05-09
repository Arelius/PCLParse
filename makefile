CSC = /Users/indy/bin/chicken4/bin/csc
.SUFFIXES: .o .scm

.scm.o:
	$(CSC) -c $(.IMPSRC)

pcl_optimize : parse.o pcl_optimize.o pcl_parser.o pcl_transform.o pcl_types.o pcl_fontutils.o pcl_writer.o
	$(CSC) $(.ALLSRC) -o $(.TARGET)
