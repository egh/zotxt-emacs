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
          'html'=> '<div style=\'line-height: 1.35; padding-left: 2em; text-indent:-2em;\' class=\'csl-bib-body\'>\n  <div class=\'csl-entry\'>Doe, John. <i>First Book</i>. Cambridge: Cambridge University Press, 2005.</div>\n  <span class=\'Z3988\' title=\'url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=book&amp;rft.btitle=First%20Book&amp;rft.place=Cambridge&amp;rft.publisher=Cambridge%20University%20Press&amp;rft.aufirst=John&amp;rft.aulast=Doe&amp;rft.au=John%20Doe&amp;rft.date=2005\'></span>\n</div>',
          'text'=> 'Doe, John. First Book. Cambridge: Cambridge University Press, 2005.'
        }
      ].to_json
    elsif format == 'easykey' 
      return ['doe:2005first'].to_json
    end
  elsif key == '0_TWCW4IJ7'
    if format == 'bibliography' && style == 'chicago-note-bibliography'
      return [
        {
          "key"=> "0_TWCW4IJ7",
          "html"=> "<div style=\"line-height: 1.35; padding-left: 2em; text-indent:-2em;\" class=\"csl-bib-body\">\n  <div class=\"csl-entry\">Doe, John, and Jenny Roe. “<i>Why Water</i> Is Wet.” In <i>Third Book</i>, edited by Sam Smith. Oxford: Oxford University Press, 2007.</div>\n  <span class=\"Z3988\" title=\"url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=bookitem&amp;rft.atitle=%3Ci%3EWhy%20Water%3C%2Fi%3E%20Is%20Wet&amp;rft.place=Oxford&amp;rft.publisher=Oxford%20University%20Press&amp;rft.aufirst=Sam&amp;rft.aulast=Smith&amp;rft.au=Sam%20Smith&amp;rft.au=John%20Doe&amp;rft.au=Jenny%20Roe&amp;rft.date=2007\"></span>\n</div>",
          "text"=> "Doe, John, and Jenny Roe. “Why Water Is Wet.” In Third Book, edited by Sam Smith. Oxford: Oxford University Press, 2007."
        }
      ].to_json
    elsif format == 'easykey'
      return ['doe:2007why'].to_json
    end
  end
  raise Sinatra::NotFound 
end
