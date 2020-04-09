\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Goober Peas"}}
  %composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  %first-page-number = #196
  %print-first-page-number = ##t
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
#(set-global-staff-size 19) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 19 20))) }
global = {
  \key d \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  
}

sopMusic = \relative c' {
  fis8. e16 fis8. g16 |
  a4 d8~ d |
  b8. ais16 b8. d16 |
  a4. \teeny g8 | \normalsize
  fis8. e16 fis8. g16 |
  a4 a |
  
  b8. a16 g8 fis |
  e4 b'\rest |
  fis8. e16 fis8. g16 |
  a8 d4~ d8 |
  d8 cis b cis |
  d4. \teeny d8 | \normalsize
  
  fis e d cis |
  b d b4\rest |
  a16 d8.\fermata cis16 e8.\fermata |
  d2 |\break
  \repeat volta 2 {
    fis,4 a |
    g b |
    
    e,8. fis16 g8 a |
    fis4 b\rest |
    fis'8. e16 d8 cis |
    b d4 b8\rest\fermata |
    a d16 b\rest cis8 e16 b\rest |
    d2
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Sit -- ting by the road -- side on a sum -- mer day, ""
  Chat -- ting with my mess -- mates pass -- ing time a -- way,
  Ly -- ing in the shad -- ow un -- der -- neath the trees, ""
  Good -- ness how de -- li -- cious, eat -- ing goo -- ber peas!
  
  Peas! Peas! Peas! Peas!
  eat -- ing goo -- ber peas!
  Good -- ness how de -- li -- cious, eat -- ing goo -- ber peas!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  When a horse -- man pass -- es, the sol -- diers have a rule,
  To cry out at their loud -- est, “Mis -- ter here’s your mule.”
  But an -- oth -- er plea -- sure en -- chant -- ing -- er than these, Is
  
  wear -- ing out your Grind -- ers, eat -- ing goo -- ber peas!
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
  d8. d16 d8. g16 |
  fis4 fis8~ fis |
  d8. d16 d8. g16 |
  fis4. \teeny g8 | \normalsize
  d8. d16 d8. g16 |
  fis4 fis |
  
  fis8. fis16 d8 d |
  cis4 s |
  d8. d16 d8. g16 |
  fis8 fis4~ fis8 |
  e8 e e e |
  d4. \teeny a'8 | \normalsize
  
  a8 a fis d |
  d b s4 |
  d16 a'8. e16 cis8. |
  d2
  \repeat volta 2 {
    d4 d |
    d d |
    
    cis8. cis16 cis8 a |
    a4 s |
    d8. fis16 fis8 fis |
    g8 eis4 s8 |
    fis8 fis16 s e8 cis16 s |
    d2
  }
}
tenorMusic = \relative c' {
  a8. a16 a8.  b16 |
  a4 a8~ a |
  g8. g16 g8. g16 |
  a4. \teeny b8 | \normalsize
  a8. a16 a8. b16 |
  a4 a |
  
  a8. a16 b8 a |
  g4 r |
  a8. a16 a8. b16 |
  a8 a4~ a8 |
  g8 g g g |
  fis4. \teeny fis8 | \normalsize
  
  d'8 d a fis |
  g8 g s4 |
  fis16 fis8. g16 g8.
  fis2 |
  \repeat volta 2 {
    a4 fis |
    g g |
    
    g8. fis16 e8 e |
    d4 s |
    a'8. a16 ais8 ais |
    b8 b4 s8 |
    d8 a16 s g8 g16 s |
    fis2
  }
}


bassMusic = \relative c' {
  d,8. d16 d8. d16 |
  d4 d8~ d |
  g,8. g16 g8. b16 |
  d4. \teeny d8 | \normalsize
  d8. d16 d8. d16 |
  d4 d |
  
  d8. d16 d8 a |
  a4 d\rest |
  d8. d16 d8. d16 |
  d8 d4~ d8 |
  a8 a a a |
  d4. \teeny d8 | \normalsize
  
  
  d8 d d d |
  g, g d'4\rest |
  d16 d8.\fermata a16 a8.\fermata |
  d2 |
  \repeat volta 2 {
    d4 d |
    b g |
    
    a8. a16 a8 cis |
    d4 d\rest |
    d8. d16 d8 d |
    g,8 gis4 d'8\rest\fermata |
    a8 a16 d\rest a8 a16 d\rest |
    d2
  }
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
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 120
    \set Staff.midiInstrument = "flute"
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.4
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
