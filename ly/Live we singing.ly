\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Live we singing"}}
  composer = \markup\oldStyleNum"Moritz Hauptmann (1792–1868)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 80))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #6
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key d \major
  \time 2/4
  \dynamicUp
  \tempo 4 = 92
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	a'8->\mf fis b g |
  a fis b g |
  a-> fis\< d d' |
  cis[\> b] a4\! |
  
  g8-> b e, g |
  fis a d4 |
  cis8-> a e gis |
  a b->[ a] g |
  fis d g e |
  
  fis d g e |
  fis a d\< fis, |
  a[\> g]\! fis4 |
  e8 d cis e |
  d4. e16[ fis] |
  
  e8 a cis b |
  a g->[ fis] e |
  d d'4-- d8 |
  d4.( cis16)[ b] |
  
  a8 d4--\< d8 |
  d8.[\> e16] fis8--\! d |
  cis b a b16[ cis] |
  d4 d, |
  
  r8 cis' e d |
  cis r8 a4->~ |
  a8 fis b g |
  a fis b g |
  
  a-> fis d\< d' |
  cis[\> b] a4\! |
  g8 b e, g |
  fis a d4 |
  cis8-> a e gis |
  
  a b->[ a] g |
  fis d g e |
  fis d g e |
  fis a d\< fis, |
  
  a[\> g] fis4\! |
  e8 d cis e |
  d[\< fis] a d |
  fis4->\f e-> d2 \bar"|."
}
sopWords = \lyricmode {
	Live we sing -- ing, danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Live we but for hap -- pi -- ness,
  Not for care and grief;
  
  Live we sing -- ing, live we danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Live we but for hap -- pi -- ness,
  Not care and grief,
  
  Live we sing -- ing, live sing -- ing, live we sing -- ing,
  And al -- ways full of plea -- sure,
  Not care and grief,
  Live __ we sing -- ing, danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Live we but for hap -- pi -- ness,
  Not for care and grief,
  Live we sing -- ing, live we danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Al -- ways full of plea -- sure,
  Not care and grief.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 8 r2 |
  a'8-> fis b g |
  a fis b g |
  a-> fis d\< d' |
  cis[\> b]\! a4 |
  
  g8-> b e, g |
  fis a d4 |
  cis8-> a e gis |
  a b->[ a] g |
  fis d g e |
  
  fis d g e |
  fis a\< d fis, |
  a[\> g] fis4\! |
  e8 d cis e |
  d4. e16[ fis] |
  
  e8 a cis b |
  a g->[ fis] e |
  d d'4-- d8 |
  d4.( cis16)[ b] |
  
  a8 d4-- d8 |
  d8.[\> e16] fis8--\! d |
  cis b a b16[ cis] |
  d4 d, |
  
  r8 cis' e d |
  cis r8 a4->~ |
  a8 fis b g |
  a fis b g |
  
  a-> fis d\< d' |
  cis[\> b] a4\! |
  g8 b e, g |
  fis[\< a] d d |
  d4->\f cis-> |
  d2 \bar"|."
}
altoWords = \lyricmode {
	Live we sing -- ing, danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Live we but for hap -- pi -- ness,
  Not for care and grief;
  
  Live we sing -- ing, live we danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Live we but for hap -- pi -- ness,
  Not care and grief,
  
  Live we sing -- ing, live sing -- ing, live we sing -- ing,
  And al -- ways full of plea -- sure,
  Not care and grief,
  Live we sing -- ing, danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Al -- ways full of plea -- sure,
  Not care and grief.
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
  \repeat unfold 16 r2 |
  a8->\mf fis b g |
  a fis b g |
  
  a-> fis\< d d' |
  cis8[\> b] a4\! |
  g8-> b e, g |
  fis a d4 |
  
  cis8-> a e gis |
  a b->[ a] g |
  fis d g e |
  fis d g e |
  
  fis a d\< fis, |
  a[\> g] fis4\! |
  e8-> d cis e |
  d4. e16[ fis] |
  e8 a cis b |
  
  a8 g->[ fis] e |
  d d'4-- d8 |
  d4.( cis16)[ b] |
  a8 d4-- d8 |
  
  <d d,>8.\>[ <e e,>16] <fis fis,>8\! d |
  cis-> b a b16[ cis] |
  d8[\< a] fis d |
  a'4\f-> << {g-> | fis2} \\ {a4-> | d,2} >> 
  \bar"|."
}

tenorWords = \lyricmode {
	Live we sing -- ing, danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Live we but for hap -- pi -- ness,
  Not for care and grief;
  
  Live we sing -- ing, live we danc -- ing, spring -- ing,
  Al -- ways full of plea -- sure,
  Live we but for hap -- pi -- ness,
  Not care and grief,
  
  Live we sing -- ing, live sing -- ing, live we sing -- ing,
  And al -- ways full of plea -- sure,
  \set associatedVoice = "sopranos"
  Not care and grief.
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  r2
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
    \new Staff = sopranos <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "sopranos"  \lyricsto "sopranos" \sopWords
    \new Staff = altos <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "altos" \altoWords
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 92
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
}


