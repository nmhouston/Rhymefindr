# Rhymefindr

This repo contains Rhymefindr, a set of R scripts designed to identify rhymes in nineteenth-century English poetry by operationalizing the rules presented in an 1824 edition of John Walker’s _A_Rhyming_Dictionary_, one of the leading references on rhyme throughout the nineteenth century. 

This repo contains three files: 

(1) The get_endwords R script extracts the endwords from plain text files of poems. The script outputs a csv with the following information for each poem: 
* the textid (filename)
* the number of lines in the poem
* the number of stanzas in the poem
* the number of lines per stanza if they are all the same length
* a character vector of the endwords from each line of the poem

(2) The find_rhymes R script is designed to work with an input csv containing the textid and a character vector of endwords for each poem. The final syllable of each endword is extracted with regular expressions and is used as the basis for a series of lookups in the key-value table created from Walker’s dictionary. The script outputs a csv with the following information for each poem: 
* a representation of the rhymes used in the poem as a sequence of capital letters
* a vector of the rhyme syllables
* a vector of the rhyme words
* a numerical vector showing which of the rhymes are perfect rhymes
* a categorical indicator for the likelihood of the poem being rhymed 

(3) A csv with a key-value table created from an 1824 edition of John Walker’s  _A_Rhyming_Dictionary;_Answering,_at_the_Same_Time,the_Purposes_of_Spelling_and_Pronouncing_the_English_Language,_on_a_Plan_not_Hitherto_Attempted_ (first published in 1775).
Keys are from the rhyme syllables that head entries in Walker's dictionary. Values are the perfect rhyme syllables, perfect rhyme words, allowable rhyme syllables, and allowable rhyme words.  The following editorial practices were followed in preparing this file: 
* there are a few words Walker lists as "nearly perfect" which were combined with the "perfect" rhyme words 
* cross-referenced rhyme syllables were duplicated to cross-reference from both entries (this was inconsistent in Walker's dictionary) 
* modern spellings for one-syllable past participles were added to make this applicable to a range of nineteenth-century texts (Thus "missed" was added where the dictionary lists "miss'd")
