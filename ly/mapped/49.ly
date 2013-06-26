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
  first-page-number = #49
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
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  d'16. d32 |
  c8 a f b16.\rest g32 |
  f16. e32 d16. e32 f8 d'16. d32
  c8 a f b16.\rest g32 |
  
  f16. e32 d16. e32 f8 a16. a32 |
  g8 bes16. bes32 a8 c16. c32 |
  bes8 d16. d32 c8 f16. e32 |
  
  e16 d c bes a8 d16 c |
  c b a b c8\fermata c16. c32 |
  c8 aes16. f32 e8 c16. c32 |
  f8 aes c\fermata d16. d32 |
  
  c8 a? f f16[ g] |
  f16. e32 d16. e32 f8 d'16. d32 |
  c8 a f'\fermata f,16. g32 |
  a4\fermata a16[ g8] f16 |
  f4 bes8\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	By the sad sea waves, I lis -- ten while they moan
  A la -- ment o’er graves of hope and plea -- sure gone.
  I was young, I was fair,
  I had once not a care,
  From the ris -- ing of the morn to the set -- ting of the sun;
  Yet I pine like a slave by the sad sea wave.
  Come a -- gain, bright days of hope and plea -- sure gone,
  Come a -- gain, bright days, Come a -- gain, come a -- gain.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  From my care last night by ho -- ly sleep be -- guiled,
  In the fair dream -- light my home up -- on me smiled.
  Oh, how sweet ’mid the dew,
  Ev -- ’ry flow’r that I knew,
  Breathed a gen -- tle wel -- come back to the worn and wear -- y child.
  I a -- wake in my grave by the sad sea wave.
  Come a -- gain, dear dream so peace -- ful -- ly that smiled,
  Come a -- gain, dear dream, Come a -- gain, come a -- gain.
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
  f16. f32 |
  f8 f c s16. c32 |
  bes16. bes32 bes16. bes32 a8 f'16. f32 |
  f8 f c s16. c32 |
  
  bes16. bes32 bes16. bes32 a8 f'16. f32 |
  e8 g16. g32 f8 a16. a32 |
  g8 bes16. bes32 a8 a16. a32 |
  
  bes16 bes g g f8 f16 f |
  f f f f e8 e16. e32 |
  f8 f16. c32 c8 c16. c32 |
  c8 f f f16. f32 |
  
  f8 f c c |
  bes16. bes32 bes16. bes32 a8 f'16. f32 |
  f8 f a c,16. e32 |
  f4 e8. f16 |
  f4 s8 \bar"|."
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
  bes16. bes32 |
  a8 c a s16. a32 |
  g16. g32 g16. g32 f8 bes16. bes32 |
  a8 c a s16. a32 |
  
  g16. g32 g16. g32 f8 c'16. c32 |
  c8 c16. c32 c8 c16. c32 |
  c8 c16. c32 c8 c16. c32 |
  
  bes16 bes c c c8 d16 d |
  d d d d c8 bes16. bes32 |
  aes8 c16. aes32 g8 bes16. bes32 |
  aes8 c aes bes16. bes32 |
  
  a?8 c a a |
  g16. g32 g16. g32 f8 bes16. bes32 |
  a8 c c a16. c32 |
  c4 c16[ bes8] a16 |
  a4 s8 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,16. f32 |
  f8 f f d16.\rest f32 |
  c16. c32 c16. c32 f,8 f'16. f32 |
  f8 f f d16.\rest f32 |
  
  c16. c32 c16. c32 f8 f16. f32 |
  c8 e16. e32 f8 f16. f32 |
  c8 e16. e32 f8 f16. f32 |
  
  bes16 bes e, e f8 d16 d |
  g g g g c,8\fermata c16. c32 |
  c8 c16. c32 c8 c16. c32 |
  f8 f f\fermata bes16. bes32 |
  
  f8 f f f |
  c16. c32 c16. c32 f,8 f'16. f32 |
  f8 f f\fermata c16. c32 |
  c4\fermata c8. f16 |
  f4 d8\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"By the Sad Sea Waves"}}
  composer = \markup\oldStyleNum"Sir Julius Benedict (1804–1885)"
  tagline = ""
}}



