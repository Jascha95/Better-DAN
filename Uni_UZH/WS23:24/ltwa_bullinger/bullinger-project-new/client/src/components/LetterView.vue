<template>
    <main class="container" v-if="letter">
        <div class="bg-light p-5 rounded">
            <h1>Brief {{ letter.id }}</h1>
            <!--      <p class="lead">This example is a quick exercise to illustrate how fixed to top navbar works. As you scroll, it will remain fixed to the top of your browser’s viewport.</p>-->
            <p class="lead">Datum: {{ letter.date }}</p>
            <p class="lead">Absender: {{ letter.sender }}</p>
            <p class="lead">Ort: {{ letter.place }}</p>
        </div>
        <div class="row">
            <div class="col-6">
                <img :src="image" alt="letter" class="img-fluid" v-for="image in letter.images" :key="image">
            </div>
            <div class="col-6">
                <div class="d-flex justify-content-end">
                    <button type="button" class="btn btn-primary" @click="toggleAnnotations">Anmerkungen
                        {{ showAnnotation ? 'ausblenden' : 'einblenden' }}</button>
                </div>
                <p class="lead">Inhalt:</p>
                <p v-html="letter.content" ref="contentContainer"></p>
            </div>
        </div>
    </main>
</template>

<script>
export default {
    props: ['id'],
    data() {
        return {
            letter: null,
            lexicalFootnotes: [],
            xmlFootnotes: [],
            showAnnotation: false
        }
    },
    mounted() {
        console.log('Component mounted.')
        this.loadLetter(this.id)
        this.loadLexicalFootnotes()
    },
    methods: {
        loadLetter(filename) {
            var self = this
            var xmlHttp = new XMLHttpRequest();
            xmlHttp.onreadystatechange = function () {
                if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
                    // parse XML
                    var parser = new DOMParser();
                    var xmlDoc = parser.parseFromString(xmlHttp.responseText, "text/xml");
                    console.log(xmlDoc)
                    // Extract data from XML according to its structure
                    let letter = {
                        id: xmlDoc.querySelector('metadata').getAttribute('id'),
                        link: filename
                    }
                    self.letter = letter

                    let year = (xmlDoc.querySelector('metadata date min year').textContent)
                    let month = (xmlDoc.querySelector('metadata date min month').textContent)
                    let day = (xmlDoc.querySelector('metadata date min day').textContent)
                    letter.date = day + '. ' + month + '. ' + year

                    let senderName = xmlDoc.querySelector('metadata senders sender person name').textContent
                    let senderSurname = xmlDoc.querySelector('metadata senders sender person forename').textContent
                    self.letter.sender = senderName + ' ' + senderSurname

                    self.letter.place = xmlDoc.querySelector('metadata senders sender place').textContent
                    let scans = xmlDoc.querySelectorAll('metadata scan_pages')
                    let images = []
                    for (let i = 0; i < scans.length; i++) {
                        let scan = scans[i].querySelector('scan')
                        // sample https://iiif.bullinger-digital.ch/iiif/3/12901%2FOriginal%2F01%20E_II_337__fol__387_v_.tif/0,0,2883,2839/2883,2839/0/default.jpg
                        // https://iiif.bullinger-digital.ch/iiif/3/12897%2FAutograph%2F00%20E_II_366__fol__237_r_.tif/0,2048,2048,2048/512,512/0/default.jpg
                        // https://iiif.bullinger-digital.ch/iiif/3/12897%2FAutograph%2F00%20E_II_366__fol__237_r_.tif/0,0,2883,2839/2883,2839/0/default.jpg
                        let link = 'https://iiif.bullinger-digital.ch/iiif/3/12897%2FAutograph%2F' + encodeURI(scan.textContent) + '/0,0,2048,2048/512,512/0/default.jpg'
                        images.push(link)
                    }
                    self.letter.images = images

                    let footnotes = xmlDoc.querySelectorAll('footnotes num fn')
                    let xmlFootnotes = []
                    for (let i = 0; i < footnotes.length; i++) {
                        let footnote = footnotes[i]
                        let id = footnote.getAttribute('ref')
                        let content = footnote.innerHTML
                        xmlFootnotes.push({
                            id: id,
                            content: content
                        })
                    }
                    self.xmlFootnotes = xmlFootnotes


                    let content = xmlDoc.querySelector('letter')
                    let contentText = content.innerHTML
                    // replace all <s to <span
                    contentText = contentText.replace(/<s/g, '<span')
                    contentText = contentText.replace(/<\/s>/g, '</span>')

                    // replace all <fl>id</fl> to <sup class="tip footnote" id="footnote-id">ootnote-id<span class="tooltiptext">Tooltip text</span></sup>
                    contentText = contentText.replace(/<fl>(.*?)<\/fl>/g, '<span style="display: none" class="tip footnote" id="$1">$1<span class="tooltiptext">Toolt text</span></span>')

                    self.letter.content = contentText
                }
            }
            xmlHttp.open("GET", '/letters/' + filename + '.xml', true); // true for asynchronous
            xmlHttp.send(null);
        },
        loadLexicalFootnotes() {
            var self = this
            var xmlHttp = new XMLHttpRequest();
            xmlHttp.onreadystatechange = function () {
                if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
                    self.lexicalFootnotes = JSON.parse(xmlHttp.responseText)
                }
            }
            xmlHttp.open("GET", 'http://127.0.0.1:5000/lexical_footnotes', true); // true for asynchronous
            xmlHttp.send(null);
        },
        toggleAnnotations() {
            this.showAnnotation = !this.showAnnotation;
            this.$nextTick(this.adjustFootnoteVisibility);
        },
        adjustFootnoteVisibility() {
            const contentContainer = this.$refs.contentContainer;

            // Query for all <sup> tags inside the content container
            const supTags = contentContainer.querySelectorAll('.footnote');
            // Toggle visibility based on the showAnnotation flag
            supTags.forEach((supTag) => {
                //get parent content
                let parentContent = supTag.parentElement.innerHTML
                //get word right before the sup tag
                let word = parentContent.substring(0, parentContent.indexOf(supTag.outerHTML))

                var footnoteId = supTag.getAttribute('id')
                footnoteId = footnoteId.replace('footnote-', '')
                //get footnote content
                let footnoteContent = this.xmlFootnotes.find(footnote => footnote.id === footnoteId)
                if (footnoteContent) {
                    footnoteContent = footnoteContent.content
                } else {
                    footnoteContent = this.lexicalFootnotes.find(footnote => footnote[0] == word)
                    if (footnoteContent) {
                        footnoteContent = footnoteContent[0]
                    } else {
                        footnoteContent = ''
                    }
                }

                supTag.querySelector('.tooltiptext').innerHTML = footnoteContent

                //get content right after the footnote
                supTag.style.display = this.showAnnotation ? 'inline' : 'none';
            });
        },
    },
}
</script>

<style >
.tip {
    border-bottom: 1px dotted black;
}

.tip .tooltiptext {
    visibility: hidden;
    min-width: 120px;
    max-width: 500px;
    max-height: 500px;

    background-color: black;
    color: #fff;
    text-align: center;
    border-radius: 6px;
    padding: 5px 0;

    /* Position the tooltip */
    position: absolute;
    z-index: 1;
}

.footnote {
    vertical-align: sub;
    font-size: smaller;
    position: relative;
    top: -0.5em;
    color: blue;
    text-decoration: underline;
    cursor: pointer;
}

.tip:hover .tooltiptext {
    visibility: visible;
}
</style>
