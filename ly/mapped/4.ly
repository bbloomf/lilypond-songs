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
  inner-margin = 0.98\in
  outer-margin = 0.73\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #4
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
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	r2 b'4. c8 |
  d1 |
  r2 d2 |
  e d |
  c2. c4 |
  b1 |
  
  r2 d |
  d c |
  b b |
  a1 |\break
  r2 a |
  b g |
  a2. a4 |
  a1 |
  
  \repeat volta 2 {
    r4 d, g2 |
    r4 e a2 |
    r4 fis b2 |
    r4 g c2 |
    r4 a d2~ |
    d1~ |
    
    d1 |
    r4 d c b |
    a2 r4 b |
    a g g2~ |
    g4( fis8[ e]) fis2 |
    g1
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Come a -- gain! sweet love doth now in -- vite
  thy grac -- es that re -- frain
  To do me due de -- light;
  
  To see, to hear, to touch, to kiss, to die, __
  
  with thee a -- gain in sweet -- est sym -- pa -- thy.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Come a -- gain! that I may cease to mourn
  through thine un -- kind dis -- dain;
  For now left and for -- lorn,
  
  I sit, I sigh, I weep, I faint, I die, __
  in dead -- ly pain and end -- less mis -- er -- y.
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
  g'2. g4 |
  g1 |
  r2 b |
  a4 g g2~ |
  g fis |
  g1 |
  
  r2 g |
  g e4.( fis8) |
  g2. g4 |
  fis1 |
  r2 fis |
  g2. d4 |
  e2. e4 |
  fis1 |
  
  \repeat volta 2 {
    d1 |
    e2 r4 e4 |
    fis2 r4 fis4 |
    g2 r4 g |
    a2 r4 a |
    b1~ |
    
    b2 a |
    g4. f8 e4 g |
    fis?2. g4 |
    e2 b4( c) |
    d2. c4 |
    b1
  }
}
altoWords = \lyricmode{
  \set stanza = #"1. "
	Come a -- gain! sweet love doth now __ in -- vite
  thy grac -- es __ that re -- frain
  To do me due de -- light;
  
  To see, to hear, to touch, to kiss, to die, __
  to die with thee a -- gain in sweet -- est __ sym -- pa -- thy.
}

altoWordsII = \lyricmode {
  \set stanza = #"2. "
  Come a -- gain! that I may cease __ to mourn
  through thine un -- kind dis -- dain;
  For now left and for -- lorn,
  
  I sit, I sigh, I weep, I faint, I die, __
  I die in dead -- ly pain and end -- less __ mis -- er -- y.
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
  d2. d4 |
  b1 |
  b2\rest b |
  c d |
  e2. d8[ c] |
  d1 |
  
  b2\rest b |
  b a |
  g d' |
  d1 |
  b2\rest d |
  d2. d4 |
  d2 cis |
  d1 |
  
  \repeat volta 2 {
    g,1 |
    g2 b4\rest c |
    a2 b4\rest d |
    b2 b4\rest e |
    d2. c4 |
    b g g a |
    
    b2 c |
    d b4\rest g |
    d'2. d4 |
    c b b a8[ g] |
    a2. a4 |
    g1
  }
}

tenorWords = \lyricmode{
  \set stanza = #"1. "
	Come a -- gain! sweet love doth now in -- vite
  thy grac -- es that re -- frain
  To do me due de -- light;
  
  To see, to hear, to touch, to kiss, to die,
  to die with thee a -- gain, with thee a -- gain in sweet -- est sym -- pa -- thy.
}

tenorWordsII = \lyricmode {
  \set stanza = #"2. "
  Come a -- gain! that I may cease to mourn
  through thine un -- kind dis -- dain;
  For now left and for -- lorn,
  
  I sit, I sigh, I weep, I faint, I die,
  I die in dead -- ly pain, in dead -- ly pain and end -- less mis -- er -- y.
}
tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g2 g |
  g1 |
  d2\rest  g, |
  c b |
  a2. a4 |
  g1 |
  
  d'2\rest g, |
  g a |
  b g |
  d'1 |
  d2\rest d |
  g, b |
  a2. a4 |
  d1 |
  
  \repeat volta 2 {
    b1 |
    c2. c4 |
    d2. d4 |
    e2. e4 |
    fis2. fis4 |
    g2 g, |
    
    g a |
    b c |
    d b |
    c4( d) e2 |
    d2. d4 |
    g,1
  }
}

bassWords = \lyricmode{
  \set stanza = #"1. "
	Come a -- gain! sweet love doth now in -- vite
  thy grac -- es that re -- frain
  To do me due de -- light;
  
  To see, to hear, to touch, to kiss, to die,
  to die with thee a -- gain in sweet -- est sym -- pa -- thy.
}

bassWordsII = \lyricmode {
  \set stanza = #"2. "
  Come a -- gain! that I may cease to mourn
  through thine un -- kind dis -- dain;
  For now left and for -- lorn,
  
  I sit, I sigh, I weep, I faint, I die,
  I die in dead -- ly pain and end -- less mis -- er -- y.
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = sop <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "sop"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "sopII"  \lyricsto "sopranos" \sopWordsII
    \new Staff = altos <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "alt"  \lyricsto "altos" \altoWords
    \new Lyrics = "altII"  \lyricsto "altos" \altoWordsII
    \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
    >>
    \new Lyrics \lyricsto "tenors" \tenorWords
    \new Lyrics \lyricsto "tenors" \tenorWordsII
    \new Staff = basses <<
      \clef bass
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \lyricsto "basses" \bassWords
    \new Lyrics \lyricsto "basses" \bassWordsII
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Come again, sweet love"}}
  composer = \markup\oldStyleNum"John Dowland (1563–1626)"
  tagline = ""
}}




