<template>
    <main class="container">
        <div class="bg-light p-5 rounded text-center">
            <h1>Bullinger Digital</h1>
            <!--      <p class="lead">This example is a quick exercise to illustrate how fixed to top navbar works. As you scroll, it will remain fixed to the top of your browser’s viewport.</p>-->
            <p class="lead">Demo Sample Text Here</p>

            <input type="text" v-model="search" placeholder="Search...">

            <div class="row">
                <div class="col-4" v-for="letter in filteredLetters" :key="letter.id">
                    <div class="card">
                        <a :href="'/letter-read/' + letter.link" class="stretched-link"></a>
                        <div class="card-body">
                            <h5 class="card-title">Brief {{ letter.id }}</h5>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </main>
</template>

<script>
export default {
    data() {
        return {
            letterFilenames: [
                '19_2848_12897.xml',
                '19_2839_12888.xml',
                '19_2840_12889.xml',
                '19_2841_12890.xml',
                '19_2842_12891.xml',
                '19_2843_12892.xml',
                '19_2844_12893.xml',
                '19_2845_12894.xml',
                '19_2846_12895.xml'
            ],
            letters: [],
            search: ''
        }
    },
    computed: {
        filteredLetters() {
            return this.letters.filter(letter => {
                return letter.id.toLowerCase().includes(this.search.toLowerCase())
                    || letter.content.toLowerCase().includes(this.search.toLowerCase())
            })
        }
    },
    mounted() {
        console.log('Component mounted.')
        this.loadLetters()
    },
    methods: {
        loadLetters() {
            this.letters = []
            for (let i = 0; i < this.letterFilenames.length; i++) {
                this.loadLetter(this.letterFilenames[i])
            }
        },
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
                    var letter = {};
                    letter.id = xmlDoc.querySelector('metadata').getAttribute('id')
                    letter.link = filename.replace('.xml', '')
                    letter.content = xmlDoc.querySelector('letter').textContent
                    self.letters.push(letter)
                }
            }
            xmlHttp.open("GET", "/letters/" + filename, true); // true for asynchronous
            xmlHttp.send(null);
        }
    }
}
</script>

<style scoped>

</style>
