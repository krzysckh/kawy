OL=ol-small

.PHONY: all clean

all: build.scm
	$(OL) -i third-party/robusta -r build.scm
clean:
	rm -f public/index.html
