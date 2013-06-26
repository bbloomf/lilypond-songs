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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #140
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 2.
  a'4 g a |
  bes2. a4 |
  g a f4. g8 |
  g4( a2.)~ |
  a4 a g a |
  bes2. a4 |
  g a f4. g8 |
  a1~ |
  
  a4 c c c |
  d2. a4 |
  a c c4. g8 |
  g4( bes2.)~ |
  bes4 bes a g |
  a2. f4 |
  f g g4. a8 |
  a1~ |
  
  a4 c c c |
  d2. a4 |
  a c c4. g8 |
  g4( bes2.)~ |
  bes4 bes a g |
  a2. f4 |
  f g g4. f8 |
  f1~ |
  f4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Be still, my soul; the Lord is on thy side; __
  Bear pa -- tient -- ly the cross of grief or pain; __
  Leave to thy God to or -- der and pro -- vide; __
  In eve -- ry change He faith -- ful will re -- main. __
  Be still, my soul; thy best, thy heav’n -- ly Friend __
  Through thorn -- y ways leads to a joy -- ful end. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Be still, my soul; thy God doth un -- der -- take __
  To guide the fu -- ture as He has the past. __
  Thy hope, thy con -- fid -- ence, let no -- thing shake; __
  All now mys -- te -- rious shall be bright at last. __
  Be still, my soul; the waves and winds still know __
  His voice who ruled them while He dwelt be -- low. __
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Be still, my soul, when dear -- est friends de -- part __
  And all is dark -- ened in the vale of tears; __
  Then shalt thou bet -- ter know His love, His heart, __
  Who comes to soothe thy sor -- rows and thy fears. __
  Be still, my soul; thy Je -- sus can re -- pay __
  From His own full -- ness all He takes a -- way. __
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Be still, my soul; the hour is hast -- ’ning on __
  When we shall be for -- ev -- er with the Lord, __
  When dis -- ap -- point -- ment, grief, and fear are gone, __
  Sor -- row for -- got, love’s pur -- est joys re -- stored. __
  Be still, my soul; when change and tears are past, __
  All safe and bless -- ed we shall meet at last. __
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 2.
  f4 e f |
  e2. f4 |
  e f d4. e8 |
  e4( f2.)~ |
  f4 f e f |
  e2. f4 |
  e f d4. e8 |
  f1~ |
  
  f4 f f f |
  f2. f4 |
  f f f4. e8 |
  e4( g2.)~ |
  g4 g fis g |
  f?2. f4 |
  f f e4. e8 |
  e1~ |
  
  e4 a f f |
  f2. f4 |
  f f f4. e8 |
  e4( g2.)~ |
  g4 g fis g |
  f?2. c4 |
  f f e4. f8 |
  f1~ |
  f4 \bar"|."
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
  \partial 2.
  c4 c c |
  c2. c4 |
  c c bes4. bes8 |
  c1~ |
  c4 c c c |
  c2. c4 |
  c c bes4. bes8 |
  c1~ |
  
  c4 a a a |
  a2. d4 |
  d c c4. c8 |
  c4( d2.)~ |
  d4 d c d |
  c2. c4 |
  d d d4. cis8 |
  cis1~ |
  
  cis!4 c a a |
  a2. d4 |
  d c c4. c8 |
  c4( d2.)~ |
  d4 d c d |
  c2. a4 |
  a4 bes bes4. a8 |
  a1~ |
  a4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 2.
  f,4 bes a |
  g2. f4 |
  bes a bes4. g8 |
  g4( f2.)~ |
  f4 f bes a |
  g2. f4 |
  bes a bes4. g8 |
  f1~ |
  
  f4 f f e |
  d2. d4 |
  d a a4. c8 |
  c4( g2.)~ |
  g4 g a bes |
  a2. c'4 |
  bes bes bes4. cis8 |
  a1~ |
  
  a4 f f e |
  d2. c4 |
  d a' a4. c8 |
  c4( g2.)~ |
  g4 g, a bes |
  c2. c4 |
  c c c4. f8 |
  f1~ |
  f4 \bar"|."
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
      \override LyricText #'font-size = #1.3
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Be Still, My Soul"}}
  composer = \markup\oldStyleNum{\concat{\italic"Finlandia" ", Jean Sibelius (1865–1957)"}}
  poet = \markup\oldStyleNum"Katharina von Schlegel (1697–1768)"
  meter = \markup\oldStyleNum"Translated by Jane L. Borthwick (1813–1897)"
  tagline = ""
}}



