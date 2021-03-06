from itertools import count, zip_longest

from german_tables import *

def tag(tag, *content, **fields):
    field_str = "".join(" %s=\"%s\"" % ("class" if key == "_class" else key, value) for key, value in fields.items())
    return "<%s%s>%s</%s>" % (tag, field_str, "".join(content), tag)
    
def unpack_cell(cell):
    return cell if isinstance(cell, list) else (cell, 1, 1)
    
def td(content, rowspan=1, colspan=1, **fields):
    if isinstance(content, list):
        content, colspan, rowspan = unpack_cell(content)
        
    return tag("td", *content, colspan=colspan, rowspan=rowspan, **fields)
    
def tr(*content):
    return tag("tr", *content)
    
def combine_repeats(grid, odd_vertical_combine=True):
    def convert(grid):
        """Turns each cell into [contents, colspan, rowspan]"""
        return [[[cell, 1, 1] for cell in row] for row in grid]
        
    def rotate_grid(grid):
        """Rotates the grid so that columns become rows, and colspans become rowspans"""
        def rotate_cell(cell):
            if cell:
                contents, width, height = cell
                return [contents, height, width]
            
            else:
                return None
                
        return [[rotate_cell(cell) for cell in row]
                for row in zip(*grid)]
        
    def combine_horizontally(grid, odd_combine=True):
        for row in grid:
            for colno, cell in enumerate(row):
                cell, colspan, rowspan = unpack_cell(cell)
                
                try:
                    for backtrack in range(1, colno+1):
                        if row[colno-backtrack]:
                            if row[colno-backtrack][0] == cell and row[colno-backtrack][2] == rowspan and (colno % 2 == 1 or odd_combine):
                                row[colno-backtrack][1] += colspan
                                row[colno] = None
                            
                            break
                            
                except IndexError:
                    pass
        
        return grid
        
    def combine_vertically(grid):
        return rotate_grid(combine_horizontally(rotate_grid(grid), odd_combine=odd_vertical_combine))
        
    return combine_horizontally(combine_vertically(convert(grid)))

def grid(h_labels, v_labels, grid, wide=False):
    #h_labels = h_labels[:len(grid[0])]

    if any(isinstance(label, tuple) for label in h_labels):
        h_axis = tr(
            td("", _class="pivot", rowspan=2),
            *[     td(label, _class="hlabel") if isinstance(label, str)
              else td(label[0], _class="hlabel", colspan=len(label[1]))
              for label in h_labels]
        ) + tr(
            *[td(label, _class="hlabel") for labels in h_labels
              if not isinstance(labels, str) for label in labels[1]]
        )
        
    else:
        h_axis = tr(td("", _class="pivot"), *[td(label, _class="hlabel") for label in h_labels])
    
    rows = [tr(td(label, _class="vlabel"), *[td(cell) for cell in row if cell]) for label, row in zip(v_labels, grid)]
    
    return tag("table", h_axis, *rows, _class="wide" if wide else "")
    
def process_emph(grid):
    """Placing a * in a cell emphasises all content after, or + before"""
    
    def do(cell):
        try:
            before, c, after = \
                next(cell.partition(c) for c in ["*", "+"] if c in cell)
            
            return      before + tag("em", after) if c == "*" \
                   else tag("em", before) + after
        
        except StopIteration:
            return cell
    
    return [[do(cell) for cell in row] for row in grid]

###

from flask import Flask

app = Flask(__name__)

@app.route("/")
def index():
    html = tag("link", href="static/language-grids.css", rel="stylesheet")
    
    html += tag("script", src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js")
    html += tag("script", src="static/language-grids.js")
    
    html += "<div class=\"column\">"
    html += tag("p", "Pronouns")
    html += grid(pronoun_kinds, pronoun_cases, combine_repeats(process_emph(pronouns), odd_vertical_combine=True))
    html += "<br/>"
    html += "</div><br/>"
    
    html += "<div class=\"column\">"
    
    html += tag("p", "Articles &nbsp; " + tag("label", tag("input", type="checkbox"), tag("span", "Combine with adjectives")))
    html += tag("p", "Definite:", _class="tight")
    html += grid(["M", "N", "F", "P"], cases, combine_repeats(process_emph(definite_articles)))
    html += "<br/>"
    html += tag("p", "Indefinite:", _class="tight")
    html += grid(["M", "N", "F"], cases, combine_repeats(process_emph(indefinite_articles)))
    html += "<br/>"
    
    html += "</div>"
    html += "<div class=\"column\">"
    
    html += "<p>Adjectives &nbsp; &nbsp; Split by:"
    html += tag("label", tag("input", type="radio", name="adjectives", value="by-inflection"), tag("span", "inflection class"))
    html += tag("label", tag("input", type="radio", name="adjectives", value="by-gender"), tag("span", "gender/number"))
    html += tag("label", tag("input", type="radio", name="adjectives", value="by-case"), tag("span", "case"))
    html += "</p>"
    
    def combine_articles(articleseses, adjectiveseses):
        for articleses, adjectiveses in zip(articleseses, adjectiveseses):
            if not articleses:
                yield adjectiveses
                continue
                
            yield [[art + " " + adj if art else "" for art, adj in zip_longest(articles, adjectives, fillvalue=None)]
                   for articles, adjectives in zip(process_emph(articleses), process_emph(adjectiveses))]
    
    def print_adjectives(adjectives, wide_tables=False):
        def print_grids(_class, h_labels, v_labels, data, odd_vertical_combine=True):
            html = ""
            
            for adjs, title in data:
                html += tag("p", "%s:" % title, _class="tight")
                html += grid(h_labels, v_labels,
                             combine_repeats(process_emph(adjs), odd_vertical_combine),
                             wide=wide_tables)
                html += "<br/>"
                
            return tag("div", html, _class="adjectives " + _class)
        
        html = print_grids("by-inflection", ["M", "N", "F", "P"], cases,
                           zip(adjectives, inflection_classes),
                           odd_vertical_combine=False)
        
        # I x C x G => G x C x I
        html += print_grids("by-gender", inflection_classes, cases,
                            zip([zip(*x) for x in
                                 zip(*[zip(*x) for x in adjectives])],
                                ["M", "N", "F", "P"]))
        
        # I x C x G => C x I x G
        html += print_grids("by-case",
                            ["M", "N", "F", "P"], inflection_classes,
                            zip(zip(*adjectives), cases))
        
        return html
        
    articles_and_adjectives = list(combine_articles([definite_articles, indefinite_articles, None], adjectives))
    
    html += tag("div", print_adjectives(adjectives),
                _class="adjectives-list without-articles")
    html += tag("div", print_adjectives(articles_and_adjectives, wide_tables=True),
                _class="adjectives-list with-articles")
    
    html += "</div>"
    
    return html

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=70, debug=True)