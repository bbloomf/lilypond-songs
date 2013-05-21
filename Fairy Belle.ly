\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Fairy Belle"}}
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #88
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
  \key c \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \relative c' {
  \partial 8 
  g'8 |
  g4 g8 g a c c8. a16 |
  g8 g e'8. c16 g4 b8\rest g16~ g |
  
  a4 b8 c c4 d8 e |
  e d d c d4 b8\rest g |
  g4 g8 g a c c8. a16 |
  
  g8~ g e'8. c16 g4 b8\rest g16~ g |
  a4 b8 c c b c d |
  e d a b c2 \bar"||"\break
  
  %Chorus
  e4. c8 g4 b\rest |
  a8 c c8. a16 g4 b8\rest g |
  
  a4 b8 c c4 d8 e |
  e d d8. c16 d2 |
  e4. c8 g4 b8\rest g |
  
  a c c8. a16 g4 b\rest |
  a b8 c c b c d |
  e b\rest e16 d8. c2 \bar"|."
}

sopWords = \lyricmode {
  \set stanza = #"1. "
	The pride of the vil -- lage and the fair -- est in the dell
  \set ignoreMelismata = ##t
  Is the queen of my song, and her name is Fair -- y Belle;
  The sound of her light step may be heard up -- on the hill
  Like the fall of the snow -- drop or the drip -- ping of the rill.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  She sings to the mead -- ows and she car -- ols to the streams,
  She _ laughs in the sun -- light and smiles while in her dreams,
  Her hair like the this -- tle down is borne up -- on the air,
  And her heart, like the hum -- ming bird’s, is free from ev -- ’ry care.
  
  Fair -- y Belle, gen -- tle Fair -- y Belle,
  The star of the night and the lil -- y of the day,
  Fair -- y Belle, the queen of all the dell,
  Long may she rev -- el on her bright sun -- ny way.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Her soft notes of mel -- o -- dy a -- round me sweet -- ly fall,
  Her _ eye full of love is now beam -- ing on my soul.
  The sound of that gen -- tle voice, the glance _ of that eye,
  Sur -- _ round me with rap -- ture that no oth -- er heart could sigh.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e8 |
  e4 e8 e f a a8. f16 |
  e8 e g8. e16 e4 s8 e16~ e |
  
  f4 f8 f fis4 a8 a |
  g g g g g4 s8 f |
  e4 e8 e f a a8. f16 |
  
  e8~ e g8. e16 e4 s8 e16~ e |
  f4 dis8 dis e e e g |
  g g f f e2 \bar"||"
  
  %chorus
  g4. e8 e4 s |
  f8 a a8. f16 e4 s8 e |
  
  f4 f8 f fis4 fis8 fis |
  g g g8. g16 g2 |
  g4. e8 e4 s8 e |
  
  f a a8. f16 e4 s |
  f dis8 dis e e e g |
  g s g16 f8. e2 \bar"|."
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
  c8 |
  c4 c8 c c c c8. c16 |
  c8 c c8. g16 c4 s8 c16~ c |
  
  c4 b8 c a4 fis8 fis |
  b b b a b4 s8 b |
  c4 c8 c c c c8. c16 |
  
  c8~ c c8. g16 c4 s8 c16~ c |
  d4 a8 a c b c b |
  c b a d c2 \bar"||"
  
  %Chorus
  c4. g8 c4 s |
  c8 c c8. c16 c4 s8 c |
  
  c4 b8 a a4 a8 a |
  b b b8. a16 b2 |
  c4. g8 c4 s8 c |
  
  c c c8. c16 c4 s |
  a a8 a g g e' d |
  c s b16 g8. g2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,8 |
  c4 c8 c f f f8. f16 |
  c8 c c8. c16 c4 d8\rest c16~ c |
  
  f4 g8 a d,4 d8 d |
  g g g g g4 d8\rest g |
  c,4 c8 c f f f8. f16 |
  
  c8~ c c8. c16 c4 d8\rest c16~ c |
  f4 fis8 fis g g g g |
  g g d g, c2 \bar"||"
  
  %chorus
  c4. c8 c4 d\rest |
  c8 c c8. c16 c4 d8\rest c |
  
  f4 f8 f d4 d8 d |
  g g g8. g16 g2 |
  c,4. c8 c4 d8\rest c |
  
  c c c8. c16 c4 d\rest |
  f fis8 fis g g g g |
  g d\rest g16 g8. c,2 \bar"|."
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


