# coding: utf-8
require 'sinatra'
require 'json'

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
    elsif format == 'easykey'
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
    elsif format == 'easykey'
      return ['doe:2007why'].to_json
    end
  end
  fail Sinatra::NotFound
end

get '/zotxt/search' do
  q = params[:q]
  format = params[:format]
  method = params[:method]
  content_type :json
  if q == 'doe first book' && method == 'titleCreatorYear' && format == 'bibliography'
    [
      {
        'key' => '0_ZBZQ4KMP',
        'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Doe, John. <i>First Book</i>. Cambridge: Cambridge University Press, 2005.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=book&amp;rft.btitle=First%20Book&amp;rft.place=Cambridge&amp;rft.publisher=Cambridge%20University%20Press&amp;rft.aufirst=John&amp;rft.aulast=Doe&amp;rft.au=John%20Doe&amp;rft.date=2005\'></span>\n</div>',
        'text' => 'Doe, John. First Book. Cambridge: Cambridge University Press, 2005.'
      }
    ].to_json
  elsif q == 'doe' && method == 'titleCreatorYear' && format == 'bibliography'
    [
      {
        'key' => '0_4T8MCITQ',
        'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Doe, John. “Article.” <i>Journal of Generic Studies</i> 6 (2006): 33–34.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Ajournal&amp;rft.genre=article&amp;rft.atitle=Article&amp;rft.jtitle=Journal%20of%20Generic%20Studies&amp;rft.volume=6&amp;rft.aufirst=John&amp;rft.aulast=Doe&amp;rft.au=John%20Doe&amp;rft.date=2006&amp;rft.pages=33-34&amp;rft.spage=33&amp;rft.epage=34\'></span>\n</div>',
        'text' => 'Doe, John. “Article.” Journal of Generic Studies 6 (2006): 33–34.'
      },
      {
        'key' => '0_TWCW4IJ7',
        'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Doe, John, and Jenny Roe. “<i>Why Water</i> Is Wet.” In <i>Third Book</i>, edited by Sam Smith. Oxford: Oxford University Press, 2007.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=bookitem&amp;rft.atitle=%3Ci%3EWhy%20Water%3C%2Fi%3E%20Is%20Wet&amp;rft.place=Oxford&amp;rft.publisher=Oxford%20University%20Press&amp;rft.aufirst=Sam&amp;rft.aulast=Smith&amp;rft.au=Sam%20Smith&amp;rft.au=John%20Doe&amp;rft.au=Jenny%20Roe&amp;rft.date=2007\'></span>\n</div>',
        'text' => 'Doe, John, and Jenny Roe. “Why Water Is Wet.” In Third Book, edited by Sam Smith. Oxford: Oxford University Press, 2007.'
      },
      {
        'key' => '0_ZBZQ4KMP',
        'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Doe, John. <i>First Book</i>. Cambridge: Cambridge University Press, 2005.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=book&amp;rft.btitle=First%20Book&amp;rft.place=Cambridge&amp;rft.publisher=Cambridge%20University%20Press&amp;rft.aufirst=John&amp;rft.aulast=Doe&amp;rft.au=John%20Doe&amp;rft.date=2005\'></span>\n</div>',
        'text' => 'Doe, John. First Book. Cambridge: Cambridge University Press, 2005.'
      },
      {
        'key' => '0_JQEUW7AI',
        'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Roe-Doe, John. “Hyphens,” 2015.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Adc&amp;rft.type=document&amp;rft.title=Hyphens&amp;rft.aufirst=John&amp;rft.aulast=Roe-Doe&amp;rft.au=John%20Roe-Doe&amp;rft.date=2015\'></span>\n</div>',
        'text' => 'Roe-Doe, John. “Hyphens,” 2015.'
      },
      {
        'key' => '0_AUP63UAA',
        'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Roe Doe, Jane. “Double Name,” 2015.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Adc&amp;rft.type=document&amp;rft.title=Double%20Name&amp;rft.aufirst=Jane&amp;rft.aulast=Roe%20Doe&amp;rft.au=Jane%20Roe%20Doe&amp;rft.date=2015\'></span>\n</div>',
        'text' => 'Roe Doe, Jane. “Double Name,” 2015.'
      },
      {
        'key' => '0_UCA4RC22',
        'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Doe, Jane. “Duplicated in Group Library,” 2015.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Adc&amp;rft.type=document&amp;rft.title=Duplicated%20in%20group%20library&amp;rft.aufirst=Jane&amp;rft.aulast=Doe&amp;rft.au=Jane%20Doe&amp;rft.date=2015\'></span>\n</div>',
        'text' => 'Doe, Jane. “Duplicated in Group Library,” 2015.'
      },
      {
        'key' => '0_KTWC3JF2',
        'html' => '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Doe, John. “Test Section.” In <i>Test Book Title</i>, n.d.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=bookitem&amp;rft.atitle=Test%20section&amp;rft.aufirst=John&amp;rft.aulast=Doe&amp;rft.au=John%20Doe\'></span>\n</div>',
        'text' => 'Doe, John. “Test Section.” In Test Book Title, n.d.'
      }
    ].to_json
  end
end
