# Change this to whatever lisp you'r using
LISP=sbcl
EVAL=--eval
QUIT=(uiop:quit)
SYSTEM=qbook

docs: pdf html

html:
	${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :qbook)" \
	        ${EVAL} "(asdf:oos 'asdf:load-op :${SYSTEM})" \
		    ${EVAL} "(qbook:publish-system-qbook :qbook 'qbook:html-generator \
                                           :output-directory \"./\" \
                                           :title \"${SYSTEM}\")" \
		    ${EVAL} "${QUIT}"

pdf:
	${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :qbook)" \
            ${EVAL} "(asdf:oos 'asdf:load-op :${SYSTEM})" \
		    ${EVAL} "(qbook:publish-system-qbook :qbook 'qbook:latex-generator \
                                            :output-file \"./${SYSTEM}.tex\" \
                                            :title \"${SYSTEM}\")" \
		    ${EVAL} "${QUIT}"
	pdflatex -shell-escape ${SYSTEM}.tex
	makeindex ${SYSTEM}.idx
	pdflatex -shell-escape ${SYSTEM}.tex
	rm ${SYSTEM}.aux ${SYSTEM}.log ${SYSTEM}.toc ${SYSTEM}.tex

clean:
	rm *.html qbook.pdf
