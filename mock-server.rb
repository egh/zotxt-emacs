# coding: utf-8
require 'sinatra'
require 'json'

get '/zotxt/version' do
  return { "version": "5.1.0" }.to_json
end

get '/zotxt/items' do
  key = params[:key]
  format = params[:format]
  style = params[:style]
  content_type :json
  if key == '0_ZBZQ4KMP'
    if format == 'bibliography' && style == 'chicago-note-bibliography'
      return [
        {
          'key' => '0_ZBZQ4KMP',
          'html' => "<div style=\"line-height: 1.35; padding-left: 2em; text-indent:-2em;\" class=\"csl-bib-body\">\n  <div class=\"csl-entry\">Doe, John. <i>First Book</i>. Cambridge: Cambridge University Press, 2005.</div>\n  <span class=\"Z3988\" title=\"url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=book&amp;rft.btitle=First%20Book&amp;rft.place=Cambridge&amp;rft.publisher=Cambridge%20University%20Press&amp;rft.aufirst=John&amp;rft.aulast=Doe&amp;rft.au=John%20Doe&amp;rft.date=2005\"></span>\n</div>",
          'text' => 'Doe, John. First Book. Cambridge: Cambridge University Press, 2005.'
        }
      ].to_json
    elsif format == 'citekey'
      return ['doe:2005first'].to_json
    elsif format == '248bebf1-46ab-4067-9f93-ec3d2960d0cd'
      return '{ | Doe, 2005 | | |zu:1254:ZBZQ4KMP}'
    elsif format == 'json'
      return [{ 'id' => 'http://zotero.org/users/1254/items/ZBZQ4KMP',
                'type' => 'book',
                'title' => 'First Book',
                'publisher' => 'Cambridge University Press',
                'publisher-place' => 'Cambridge',
                'event-place' => 'Cambridge',
                'note' => 'bibtex: Doe2005',
                'author' => [{ 'family' => 'Doe', 'given' => 'John' }],
                'issued' => { 'date-parts' => [['2005']] }
              }].to_json
    end
  elsif key == '0_TWCW4IJ7'
    if format == 'bibliography' && style == 'chicago-note-bibliography'
      return [
        {
          'key' => '0_TWCW4IJ7',
          'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Doe, John, and Jenny Roe. “<i>Why Water</i> Is Wet.” In <i>Third Book</i>, edited by Sam Smith. Oxford: Oxford University Press, 2007.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=bookitem&amp;rft.atitle=%3Ci%3EWhy%20Water%3C%2Fi%3E%20Is%20Wet&amp;rft.place=Oxford&amp;rft.publisher=Oxford%20University%20Press&amp;rft.aufirst=Sam&amp;rft.aulast=Smith&amp;rft.au=Sam%20Smith&amp;rft.au=John%20Doe&amp;rft.au=Jenny%20Roe&amp;rft.date=2007\'></span>\n</div>',
          'text' => 'Doe, John, and Jenny Roe. “Why Water Is Wet.” In Third Book, edited by Sam Smith. Oxford: Oxford University Press, 2007.'
        }
      ].to_json
    elsif format == 'citekey'
      return ['doe:2007why'].to_json
    end
  elsif key == '0_4T8MCITQ'
    if format == 'bibliography' && style == 'chicago-note-bibliography'
      return JSON.pretty_generate(
        [{ 'key' => '0_4T8MCITQ',
           'html' => '<div style=\"line-height: 1.35; padding-left: 2em; text-indent:-2em;\" class=\"csl-bib-body\">\n  <div class=\"csl-entry\">Doe, John. “Article.” <i>Journal of Generic Studies</i> 6 (2006): 33–34.</div>\n  <span class=\"Z3988\" title=\"url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Ajournal&amp;rft.genre=article&amp;rft.atitle=Article&amp;rft.jtitle=Journal%20of%20Generic%20Studies&amp;rft.volume=6&amp;rft.aufirst=John&amp;rft.aulast=Doe&amp;rft.au=John%20Doe&amp;rft.date=2006&amp;rft.pages=33-34&amp;rft.spage=33&amp;rft.epage=34\"></span>\n</div>',
           'text' => 'Doe, John. “Article.” Journal of Generic Studies 6 (2006): 33–34.'
         }])
    end
  end
  fail Sinatra::NotFound
end

get '/zotxt/search' do
  q = params[:q]
  format = params[:format]
  method = params[:method]
  content_type :json
  if q == 'doe first book' && method == 'titleCreatorYear'
    if format == 'json'
      return JSON.pretty_generate(
               [{ 'id' => 'http://zotero.org/users/1254/items/ZBZQ4KMP',
                  'type' => 'book',
                  'title' => 'First Book',
                  'publisher' => 'Cambridge University Press',
                  'publisher-place' => 'Cambridge',
                  'event-place' => 'Cambridge',
                  'note' => 'bibtex: Doe2005',
                  'author' => [{ 'family' => 'Doe', 'given' => 'John' }],
                  'issued' => { 'date-parts' => [['2005']] }
                }])
    elsif format == 'quickBib'
      return JSON.pretty_generate(
               [{ 'key' => '0_ZBZQ4KMP',
                  'quickBib' => 'Doe, John - First Book' }])
    end
  elsif q == 'doe' && method == 'titleCreatorYear'
    if format == 'json'
      return JSON.pretty_generate(
               [{ 'id' => 'http://zotero.org/groups/658/items/HCJIH6C2',
                  'type' => 'article',
                  'title' => 'Duplicated in group library',
                  'author' => [{ 'family' => 'Doe', 'given' => 'Jane' }],
                  'issued' => { 'date-parts' => [['2015']] } },
                { 'id' => 'http://zotero.org/users/1254/items/4T8MCITQ',
                  'type' => 'article-journal',
                  'title' => 'Article',
                  'container-title' => 'Journal of Generic Studies',
                  'page' => '33-34',
                  'volume' => '6',
                  'note' => 'bibtex: Doe2006',
                  'author' => [{ 'family' => 'Doe', 'given' => 'John' }],
                  'issued' => { 'date-parts' => [['2006']] } },
                { 'id' => 'http://zotero.org/users/1254/items/5BST32AT',
                  'type' => 'article-newspaper',
                  'title' => 'Why Does the OAS Defend the Rule of Law in Honduras but Not Venezuela?',
                  'container-title' => 'The Washington Post',
                  'source' => 'washingtonpost.com',
                  'URL' => 'http://www.washingtonpost.com/wp-dyn/content/article/2009/07/23/AR2009072303003.html',
                  'ISSN' => '0740-5421',
                  'accessed' => { 'date-parts' => [['2009', 7, 29]] } },
                { 'id' => 'http://zotero.org/users/1254/items/7HDTQQN3',
                  'type' => 'book',
                  'title' => 'How Many Machine Guns Does It Take to Cook One Meal?: The Seattle and San Francisco General Strikes',
                  'publisher' => 'University of Washington Press',
                  'number-of-pages' => '165',
                  'source' => 'Amazon.com',
                  'shortTitle' => 'How Many Machine Guns Does It Take to Cook One Meal?',
                  'author' => [{ 'family' => 'Johnson', 'given' => 'Victoria L.' }],
                  'issued' => { 'date-parts' => [['2008', 3, 1]] } },
                { 'id' => 'http://zotero.org/users/1254/items/8VPAZITB',
                  'type' => 'article-journal',
                  'title' => 'Does High Public Debt Consistently Stifle Economic Growth? A Critique of Reinhart and Rogoff',
                  'container-title' => 'Political Economy Research Institute Working Paper Series',
                  'issue' => '322',
                  'source' => 'Google Scholar',
                  'URL' => 'http://www.handelszeitung.ch/sites/handelszeitung.ch/files/article/documents/wp322.pdf',
                  'shortTitle' => 'Does High Public Debt Consistently Stifle Economic Growth?',
                  'author' => [{ 'family' => 'Herndon', 'given' => 'Thomas' },
                               { 'family' => 'Ash', 'given' => 'Michael' },
                               { 'family' => 'Pollin', 'given' => 'Robert' }],
                  'issued' => { 'date-parts' => [['2013']] },
                  'accessed' => { 'date-parts' => [['2013', 4, 29]] } },
                { 'id' => 'http://zotero.org/users/1254/items/9WIEAQCQ',
                  'type' => 'article',
                  'title' => 'Does competition among public schools benefit students and taxpayers? A comment of Hoxby',
                  'author' => [{ 'family' => 'Rothstein', 'given' => 'Jesse' }],
                  'issued' => { 'date-parts' => [['2004', 12, 15]] } },
                { 'id' => 'http://zotero.org/users/1254/items/AUP63UAA',
                  'type' => 'article',
                  'title' => 'Double Name',
                  'author' => [{ 'family' => 'Roe Doe', 'given' => 'Jane' }],
                  'issued' => { 'date-parts' => [['2015']] } },
                { 'id' => 'http://zotero.org/users/1254/items/BT26FFUM',
                  'type' => 'paper-conference',
                  'title' => 'Reliability modelling for long term digital preservation',
                  'container-title' => '9th DELOS Network of Excellence thematic workshop “Digital Repositories: Interoperability and Common Services”, Foundation for Research and Technology-Hellas (FORTH)',
                  'source' => 'Google Scholar',
                  'author' => [{ 'family' => 'Constantopoulos', 'given' => 'P.' },
                               { 'family' => 'Doerr', 'given' => 'M.' },
                               { 'family' => 'Petraki', 'given' => 'M.' }],
                  'issued' => { 'date-parts' => [['2005']] } },
                { 'id' => 'http://zotero.org/users/1254/items/HQKCK44E',
                  'type' => 'book',
                  'title' => 'The Haskell road to logic, maths and programming',
                  'publisher' => "King's College Publications",
                  'publisher-place' => 'London',
                  'event-place' => 'London',
                  'ISBN' => '978-0-9543006-9-2',
                  'author' => [{ 'family' => 'Doets', 'given' => 'Kees' }],
                  'issued' => { 'date-parts' => [['2004']] } },
                { 'id' => 'http://zotero.org/users/1254/items/JQEUW7AI',
                  'type' => 'article',
                  'title' => 'Hyphens',
                  'note' => 'bibtex: Roe-Doe2015',
                  'author' => [{ 'family' => 'Roe-Doe',
                                 'given' => 'John' }],
                  'issued' => { 'date-parts' => [['2015']] } },
                { 'id' => 'http://zotero.org/users/1254/items/KTWC3JF2',
                  'type' => 'chapter',
                  'title' => 'Test section',
                  'container-title' => 'Test book title',
                  'author' => [{ 'family' => 'Doe',
                                 'given' => 'John' }] },
                { 'id' => 'http://zotero.org/users/1254/items/MWFHJ2N8',
                  'type' => 'article-journal',
                  'title' => 'What Is a URI and Why Does It Matter?',
                  'container-title' => 'Ariadne',
                  'issue' => '65',
                  'URL' => 'http://www.ariadne.ac.uk/issue65/thompson-hs/',
                  'author' => [{ 'family' => 'Thompson',
                                 'given' => 'Henry' }],
                  'issued' => { 'date-parts' => [['2010', 10]] },
                  'accessed' => { 'date-parts' => [['2011', 3, 9]] } },
                { 'id' => 'http://zotero.org/users/1254/items/MXFJSDA5',
                  'type' => 'article-journal',
                  'title' => 'Why Does a City Grow? Specialisation, Human Capital or Institutions?',
                  'container-title' => 'Urban Studies',
                  'page' => '2027-2050',
                  'volume' => '47',
                  'issue' => '10',
                  'source' => 'usj.sagepub.com',
                  'abstract' => 'Why are there persistent differences in income between metropolitan areas? The answer to this question has evaded much of the scholarship on the topic. Some of the frameworks that drive empirical research in this field are based on ad hoc combinations of explanatory factors, ranging from natural climate, to business climate, to land and labour costs. Theoretical approaches emphasise economic specialisation: some activities have higher rates of growth than others and this translates into divergence in interurban growth and income. Yet specialisation itself needs to be explained. International economics explains different growth rates and income levels among countries by emphasising specialisation, human capital and institutions. This framework can be adapted to the analysis of metropolitan growth. The thorniest aspect of doing so is to consider recursive relationships among the three, as well as decisive events that might introduce irreversible path-dependent outcomes that differentiate cities.',
                  'URL' => 'http://usj.sagepub.com/content/47/10/2027',
                  'DOI' => '10.1177/0042098009359957',
                  'ISSN' => '0042-0980, 1360-063X',
                  'shortTitle' => 'Why Does a City Grow?',
                  'journalAbbreviation' => 'Urban Stud',
                  'language' => 'en',
                  'author' => [{ 'family' => 'Storper',
                                 'given' => 'Michael' }],
                  'issued' => { 'date-parts' => [['2010', 9, 1]] },
                  'accessed' => { 'date-parts' => [['2014', 4, 9]] } },
                { 'id' => 'http://zotero.org/users/1254/items/NJNUZID2',
                  'type' => 'post-weblog',
                  'title' => 'Why does the address bar show the tempolink instead of the permalink?',
                  'container-title' => 'W3C Blog',
                  'URL' => 'http://www.w3.org/QA/2010/04/why_does_the_address_bar_show.html',
                  'author' => [{ 'family' => 'Rees',
                                 'given' => 'Jonathan' }],
                  'issued' => { 'date-parts' => [['2010', 4, 19]] },
                  'accessed' => { 'date-parts' => [['2011', 5, 12]] } },
                { 'id' => 'http://zotero.org/users/1254/items/TWCW4IJ7',
                  'type' => 'chapter',
                  'title' => '<i>Why Water</i> Is Wet',
                  'container-title' => 'Third Book',
                  'publisher' => 'Oxford University Press',
                  'publisher-place' => 'Oxford',
                  'event-place' => 'Oxford',
                  'editor' => [{ 'family' => 'Smith', 'given' => 'Sam' }],
                  'author' => [{ 'family' => 'Doe', 'given' => 'John' },
                               { 'family' => 'Roe', 'given' => 'Jenny' }],
                  'issued' => { 'date-parts' => [['2007']] } },
                { 'id' => 'http://zotero.org/users/1254/items/UCA4RC22',
                  'type' => 'article',
                  'title' => 'Duplicated in group library',
                  'author' => [{ 'family' => 'Doe', 'given' => 'Jane' }],
                  'issued' => { 'date-parts' => [['2015']] } },
                { 'id' => 'http://zotero.org/users/1254/items/ZBZQ4KMP',
                  'type' => 'book',
                  'title' => 'First Book',
                  'publisher' => 'Cambridge University Press',
                  'publisher-place' => 'Cambridge',
                  'event-place' => 'Cambridge',
                  'note' => 'bibtex: Doe2005',
                  'author' => [{ 'family' => 'Doe', 'given' => 'John' }],
                  'issued' => { 'date-parts' => [['2005']] } },
                { 'id' => 'http://zotero.org/users/1254/items/ZNK43456',
                  'type' => 'article-journal',
                  'title' => 'Does the Past Have Useful Economics?',
                  'container-title' => 'Journal of Economic Literature',
                  'page' => '434-461',
                  'volume' => '14',
                  'issue' => '2',
                  'source' => 'JSTOR',
                  'URL' => 'http://www.jstor.org/stable/2722462',
                  'DOI' => '10.2307/2722462',
                  'ISSN' => '0022-0515',
                  'note' => 'ArticleType: research-article / Full publication date: Jun., 1976 / Copyright © 1976 American Economic Association',
                  'journalAbbreviation' => 'Journal of Economic Literature',
                  'author' => [{ 'family' => 'McCloskey', 'given' => 'Donald N.' }],
                  'issued' => { 'date-parts' => [['1976', 6, 1]] },
                  'accessed' => { 'date-parts' => [['2013', 5, 25]] } }])
    elsif format == 'quickBib'
      return JSON.pretty_generate(
        [{ 'key' => '25522_HCJIH6C2',
           'quickBib' => 'Doe, Jane - Duplicated in group library' },
         { 'key' => '0_4T8MCITQ',
           'quickBib' => 'Doe, John - Article' },
         { 'key' => '0_5BST32AT',
           'quickBib' => ' - Why Does the OAS Defend the Rule of Law in Honduras but Not Venezuela?' },
         { 'key' => '0_7HDTQQN3',
           'quickBib' => 'Johnson, Victoria L. - How Many Machine Guns Does It Take to Cook One Meal?: The Seattle and San Francisco General Strikes' },
         { 'key' => '0_8VPAZITB',
           'quickBib' => 'Herndon, Thomas - Does High Public Debt Consistently Stifle Economic Growth? A Critique of Reinhart and Rogoff' },
         { 'key' => '0_9WIEAQCQ',
           'quickBib' => 'Rothstein, Jesse - Does competition among public schools benefit students and taxpayers? A comment of Hoxby' },
         { 'key' => '0_AUP63UAA',
           'quickBib' => 'Roe Doe, Jane - Double Name' },
         { 'key' => '0_BT26FFUM',
           'quickBib' => 'Constantopoulos, P. - Reliability modelling for long term digital preservation' },
         { 'key' => '0_HQKCK44E',
           'quickBib' => 'Doets, Kees - The Haskell road to logic, maths and programming' },
         { 'key' => '0_JQEUW7AI',
           'quickBib' => 'Roe-Doe, John - Hyphens' },
         { 'key' => '0_KTWC3JF2',
           'quickBib' => 'Doe, John - Test section' },
         { 'key' => '0_MWFHJ2N8',
           'quickBib' => 'Thompson, Henry - What Is a URI and Why Does It Matter?' },
         { 'key' => '0_MXFJSDA5',
           'quickBib' => 'Storper, Michael - Why Does a City Grow? Specialisation, Human Capital or Institutions?' },
         { 'key' => '0_NJNUZID2',
           'quickBib' => 'Rees, Jonathan - Why does the address bar show the tempolink instead of the permalink?' },
         { 'key' => '0_TWCW4IJ7',
           'quickBib' => 'Smith, Sam - <i>Why Water</i> Is Wet' },
         { 'key' => '0_UCA4RC22',
           'quickBib' => 'Doe, Jane - Duplicated in group library' },
         { 'key' => '0_ZBZQ4KMP',
           'quickBib' => 'Doe, John - First Book' },
         { 'key' => '0_ZNK43456',
           'quickBib' => 'McCloskey, Donald N. - Does the Past Have Useful Economics?' }])
    end
  end
end
