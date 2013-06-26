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
  first-page-number = #126
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
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  ees16[ ees] |
  c'8 bes aes aes g aes |
  bes aes f ees b'\rest c,16[ des] |
  ees aes b8\rest ees, ees16 bes' b8\rest ees, |
  
  ees16 aes~ aes4 b4\rest ees,16 ees |
  c'8 bes aes aes g aes |
  bes aes f ees b'\rest aes16[ aes] |
  g bes b8\rest g f16 bes b8\rest f |
  
  g16 ees~ ees4 b'\rest ees,16[ f] |
  ges8. aes16 ges8 f8. e16 f8 |
  c' bes a bes b\rest ees,16[ f] |
  ges8. aes16 ges8 f8. e16 f8 |
  
  c' bes a bes b\rest bes16 c |
  des8. c16 bes8 aes f aes |
  c8. bes16 aes8 bes\fermata b8\rest ees, |
  ees16 aes b8\rest ees, f16 aes b8\rest f |
  ees16 aes~ aes4 b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	On a tree by a riv -- er a lit -- tle tom -- tit
  Sang, _ “Wil -- low, tit -- wil -- low, tit -- wil -- low!” _
  And I said to him, “Dick -- y -- bird, why do you sit
  Sing -- ing, ‘Wil -- low, tit -- wil -- low, tit -- wil -- low’?” _
  “Is it weak -- ness of in -- tel -- lect, bird -- ie?” I cried,
  “Or a ra -- ther tough worm in your lit -- tle in -- side?”
  With a shake of his poor lit -- tle head, he re -- plied,
  “Oh, wil -- low, tit -- wil -- low, tit -- wil -- low!” _
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  He _ slapped at his chest, as he sat on that bough,
  Sing -- ing, “Wil -- low, tit -- wil -- low, tit -- wil -- low!” _
  And a cold per -- spi -- ra -- tion be -- span -- gled his brow,
  Oh, _ wil -- low, tit -- wil -- low, tit -- wil -- low! _
  He __ _ sobbed and he sighed, and a gur -- gle he gave,
  Then he plunged him -- self in -- to the bil -- low -- y wave,
  And an ech -- o a -- rose from the su -- i -- cide’s grave:
  “Oh, wil -- low, tit -- wil -- low, tit -- wil -- low!” _
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Now I feel just as sure as I’m sure that my name
  Is -- n’t Wil -- low, tit -- wil -- low, tit -- wil -- low, _
  That ’twas blight -- ed af -- fec -- tion that made him ex -- claim,
  “Oh, _ wil -- low, tit -- wil -- low, tit -- wil -- low!” _
  And if you re -- main cal -- lous and ob -- du -- rate, I
  Shall _ per -- ish as he did, and you will know why,
  Though I prob -- ab -- ly shall not ex -- claim as I die,
  “Oh, wil -- low, tit -- wil -- low, tit -- wil -- low!” _
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c16[ c] |
  ees8 des c ees ees ees |
  f f des c s aes16[ bes] |
  c c s s c8 des16 des s8 des |
  
  c16 c~ c4 s c16 c |
  ees8 des c ees ees ees |
  f f des c s ees16[ ees] |
  ees ees s8 ees d16 d s8 d |
  
  ees16 ees~ ees4 s ees16[ bes] |
  ees8. ees16 ees8 d8. cis16 d8 |
  ees des ees des s ees16[ bes] |
  ees8. ees16 ees8 d8. cis16 d8 |
  
  ees des ees f s des16 ees |
  f8. ees16 des8 des des des |
  d8. d16 d8 ees s des |
  c16 c s8 c des16 des s8 des |
  c16 c~ c4 s \bar"|."
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
  aes16[ aes] |
  aes8 aes aes c c c |
  des des aes aes s ees16[ ees] |
  ees ees s8 aes g16 g s8 g |
  
  aes16 aes~ aes4 s aes16 aes |
  aes8 aes aes c c c |
  des des aes aes s c16[ c] |
  bes g s8 bes aes16 aes s8 bes |
  
  bes16 g~ g4 s g16[ aes] |
  bes8. bes16 bes8 bes8. bes16 bes8 |
  a bes c bes s g16[ aes] |
  bes8. bes16 bes8 bes8. bes16 bes8 |
  
  a bes c des s bes16 a? |
  bes8. a16 bes8 f aes f |
  aes8. aes16 bes8 g s g |
  aes16 aes s8 aes aes16 f s8 aes |
  aes16 ees~ ees4 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes,16[ aes] |
  aes8 aes aes aes' aes aes |
  des, des des aes d\rest ees16[ ees] |
  aes, aes d8\rest aes ees'16 ees d8\rest ees |
  
  aes,16 aes~ aes4 d\rest aes16 aes |
  aes8 aes aes aes' aes aes |
  des, des des aes d\rest aes'16[ aes] |
  bes, bes d8\rest bes bes16 bes d8\rest bes |
  
  ees16 ees~ ees4 d\rest ees16[ d] |
  ees8. ees16 ees8 bes8. bes16 bes8 |
  f' f f bes, d\rest ees16[ d] |
  ees8. ees16 ees8 bes8. bes16 bes8 |
  
  f' f f bes, d\rest bes16 f' |
  bes8. f16 ges8 des des des |
  bes8. bes16 bes8 ees\fermata d\rest ees |
  aes,16 aes d8\rest aes des16 des d8\rest des |
  aes16 aes~ aes4 d4\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Tit-Willow"}}
  poet = \markup\oldStyleNum"W. S. Gilbert (1836–1911)"
  composer = \markup\oldStyleNum"Arthur Sullivan (1842–1900)"
  tagline = ""
}}



