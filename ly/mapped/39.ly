\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #39
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key ees \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 8 g'16[ aes] |
  bes8. c16 bes8 bes c bes |
  ees d c bes4 \bar"" bes16[ aes] |
  
  g8 bes ees, g bes ees, |
  f8. f16 f8 f4 \bar"" g16[ aes] |
  
  bes8.[ c16] bes8 bes c bes |
  ees d c bes4 \bar"" bes16[ aes] |
  
  g8 bes bes, f'16 bes8. bes,8 |
  ees16 ees8. ees8 ees4 \bar"||"\break
  
  bes8 |
  ees16 ees8. ees8 ees8[ aes] g |
  f16 bes,8. bes8 bes4 bes8 |
  f'16 f8. f8 f g aes |
  
  aes g8 g g4 \bar"" ees16[ f] |
  g8. f16 g8 aes8.[ g16] aes8 |
  
  bes8. aes16 bes8 c4\fermata bes16 aes |
  g8 bes bes, f'16 bes8. bes,8 |
  ees16 ees8. ees8 ees4\fermata \bar"|."
}
sopWords = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"1. "
  To the Lords o’ Con -- ven -- tion ’twas Cla -- ver who spoke,
  “Ere the King’s crown go down there are crowns to be broke;
  So let each Cav -- a -- lier who loves hon -- our and me,
  Come _ fol -- low the bon -- nets o’ Bon -- nie Dun -- dee.”
}

sopWordsII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"2. "
  Dun -- _ dee he is mount -- ed, he rides up the street,
  The _ bells are rung back -- ward, the drums they are beat,
  But the pro -- vost, douce man, said, “Just e’en let it be,
  For the town is weel rid o’ that de’il o’ Dun -- dee.”
  
  \unset ignoreMelismata
  Come fill up my cup, come fill up my can,
  Come sad -- dle my hors -- es and call out my men;
  Un -- hook the West -- port and let us gang free,
  For it’s up wi’ the bon -- nets o’ Bon -- nie Dun -- dee!
}

sopWordsIII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"3. "
  There are hills be -- yond Pent -- land, and lands be -- yond Forth,
  If there’s lords in the Low -- lands, there’s chiefs in the North;
  There are brave _ Duinne -- was -- seis, three thou -- sand times three,
  Will cry, “Hie, for the bon -- nets o’ Bon -- nie Dun -- dee.”
}

sopWordsIV = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"4. "
  Then a -- wa’ to the hills, to the caves, to the rocks,
  Ere I own a u -- sur -- per I’ll couch wi’ a fox;
  And _ trem -- ble, faus Whigs, in the midst o’ your glee,
  Ye hae no seen the last o’ my bon -- nets and me!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees16[ d] |
  ees8. ees16 ees8 ees ees ees |
  aes aes ees ees4 ees16[ c] |
  
  ees8 ees ees ees ees ees |
  d8. d16 d8 d4
  
  ees16[ d] |
  ees8.[ ees16] ees8 ees ees ees |
  aes aes ees ees4 ees16[ c] |
  
  ees8 ees bes d16 d8. bes8 |
  bes16 bes8. c8 bes4 \bar"||"
  
  bes8 |
  bes16 bes8. bes8 bes[ c] bes |
  d16 bes8. bes8 bes4 bes8 |
  d16 d8. d8 d ees f |
  ees ees ees ees4 
  
  ees16[ d] |
  ees8. d16 ees8 c8.[ ees16] d8 |
  ees8. c16 ees8 ees4 ees16 c |
  ees8 ees bes d16 d8. bes8 |
  bes16 bes8. c8 bes4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  bes16[ bes] |
  g8. aes16 g8 g aes g |
  c bes aes g4 g16[ aes] |
  
  bes8 g g bes g g |
  bes8. bes16 bes8 bes4
  
  
  bes16[ bes] |
  g8.[ aes16] g8 g aes g |
  c bes aes g4 g16[ aes] |
  
  bes8 g g bes16 f8. f8 |
  g16 g8. aes8 g4 \bar"||"
  
  bes8 |
  g16 g8. g8 g[ ees] g |
  bes16 f8. f8 f4 f8 |
  bes16 bes8. bes8 bes bes bes |
  bes bes bes bes4 
  
  bes8 |
  bes8. bes16 bes8 aes8.[ bes16] bes8 |
  g8. aes16 g8 aes4 bes16 aes |
  bes8 g g bes16 f8. f8 |
  g16 g8. aes8 g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g16[ f] |
  ees8. aes,16 ees'8 ees aes, ees' |
  ees bes c ees4 ees16[ aes,] |
  
  ees'8 ees ees ees ees c |
  bes8. bes16 bes8 bes4 g'16[ f] |
  
  ees8.[ aes,16] ees'8 ees aes, ees' |
  ees bes c ees4 ees16[ aes,] |
  
  ees'8 ees ees bes16 bes8. bes8 |
  ees16 ees8. aes,8 ees'4 \bar"||"
  
  bes'8 |
  ees,16 ees8. ees8 ees[ aes,] ees' |
  bes16 d8. d8 bes4 d8 |
  bes16 bes8. bes8 bes ees d |
  
  ees ees8 ees8 ees4 g16[ f] |
  ees8. d16 ees8 ees4 f8 |
  
  ees8. ees16 ees8 aes,4\fermata g16 aes |
  ees'8 ees ees bes16 bes8. bes8 |
  ees16 ees8. aes,8 ees'4\fermata \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.0
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Bonnie Dundee"}}
  poet = \markup\oldStyleNum"Walter Scott (1771–1832)"
  composer = \markup\oldStyleNum"Old Scotch Air"
  tagline = ""
}}
