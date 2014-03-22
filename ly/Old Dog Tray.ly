\version "2.14.2"
\include "util.ly"
\header {
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Old Dog Tray"}}
  composer = \markup\oldStyleNum"Stephen Foster (1826–1864)"
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
       (padding . 2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 40))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \transpose d ees {
  \relative c' {
    \partial 8 
    d8 |
    d g fis g
    a4 a8\rest a |
    b a fis d
    d4 a'8\rest b |
    b4 ais8 b
    
    d4 c8 a |
    g~ g g8 b
    a4 a8\rest d, |
    d g fis g
    a4. a8 |
    
    b8 a fis d
    b'4. \teeny b8 \normalsize |
    b c e, a
    g4 fis |
    g2. a4\rest \bar"||"
    d4 a
    a a8 b |
    
    g2
    d4 a'\rest |
    d a8 a
    a4 g8 a |
    b2. a8\rest d,8 |
    d g fis g
    
    a4. a8 |
    b a fis d
    b'4. b8 |
    b c e, a
    g4 fis |
    g2. a8\rest \bar"|."
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The morn of life is past,
  And eve -- ning comes at last,
  It brings me a dream of a once __ hap -- py day,
  Of mer -- ry forms I’ve seen
  Up -- on the vil -- lage green, ""
  Sport -- ing with my old dog Tray.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The forms I call’d my own
  Have van -- ish’d one by one,
  The loved ones, the dear ones have all __ pass’d a -- way,
  Their hap -- py smiles are flown,
  Their gen -- tle voic -- es gone,
  I’ve no -- thing left but old dog Tray.
  
  Old dog Tray’s ev -- er faith -- ful,
  Grief can -- not drive him a -- way;
  He’s gen -- tle, he is kind,
  I’ll nev -- er, nev -- er find
  A bet -- ter friend than old dog Tray.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  When thoughts re -- call the past,
  His eyes are on me cast,
  I know that he feels what my break -- ing heart would say;
  Al -- though he can -- not speak,
  I’ll vain -- ly, vain -- ly seek
  A bet -- ter friend than old dog Tray.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c8 |
  c ees ees ees g4 s8 g |
  g g ees ees c4 s8 ees |
  ees4 ees8 ees f4 f8 f |
  
  ees~ ees ees8 ees ees4 s8 des |
  c ees ees ees g4 s8 g |
  g g ees ees ees4. \teeny ges8 \normalsize |
  
  ges f f f ees4 ees |
  ees2. s4 \bar"||"
  g4 g g g8 g |
  ees2 ees4 s |
  g g8 g g4 f8 g |
  
  aes2. s8 c, |
  c ees ees ees g4. g8 |
  g g ees ees ees4. ges8 |
  ges f f f ees4 ees |
  ees2. s8 \bar"|."
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
  aes8 |
  aes aes bes c ees4 s8 ees |
  ees des bes g aes4 s8 aes |
  aes4 aes8 aes aes4 bes8 des |
  
  c~ c aes8 aes g4 s8 g |
  aes aes bes c ees4 s8 ees |
  ees des bes g aes4. \teeny aes8 \normalsize |
  
  aes8 bes des des c4 des |
  c2. s4 \bar"||"
  bes4 ees ees ees8 ees |
  c2 c4 s |
  bes ees8 ees ees4 c8 ees |
  
  ees2. s8 aes,8 |
  aes aes bes c ees4. ees8 |
  ees des bes g aes4. aes8 |
  aes bes des des c4 des |
  c2. s8 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes,8 |
  aes c ees ees ees4 d8\rest ees |
  ees ees ees ees aes,4 d8\rest aes'|
  aes4 aes8 aes des,4 bes8 bes |
  
  ees~ ees c8 aes ees'4 d8\rest ees |
  aes, c ees ees ees4 d8\rest ees |
  ees ees ees ees aes,4. \teeny aes8 \normalsize |
  
  aes des des bes ees4 ees |
  aes2. d,4\rest \bar"||"
  ees4 ees ees ees8 ees |
  aes2 aes4 d,\rest |
  ees ees8 ees ees4 ees8 ees |
  
  aes2. d,8\rest aes'|
  aes c, ees ees ees4. ees8 |
  ees ees ees ees aes,4. aes8 |
  aes des des bes ees4 ees |
  aes2. d,8\rest \bar"|."
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
  \midi {
    \tempo 4 = 105
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


