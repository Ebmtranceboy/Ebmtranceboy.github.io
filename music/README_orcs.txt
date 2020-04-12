To register a new ORC:
---------------------

* make sure its content ends with at a new line

* add its name as a new line in service-worker.js:
 
      var filesToCache = [
        ...
        "/orcs/MyGloriousInstr.orc",
        ...
       ];

* declare it at the beginning of the index.html last <script>:
 
      let myGloriousInstrOrc='';

* load it on the layoutComplete() function of that script:

      fetch("orcs/MyGloriousInstr.orc").then(function(response) {
          return response.text().then(function(v) {
            myGloriousInstrOrc = v;
             })
           });
           
 * use it at the end of the restartPref() last fonction:
 
      cs.compileOrc(...+myGloriousInstrOrc+...+code.value);
      
      
